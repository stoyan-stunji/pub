#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Режим на работа на машината: 0 - user, 1 - supervisor:
typedef enum { 
    MODE_USER = 0, 
    MODE_KERNEL = 1 
} Mode;

// S = <E, M, P, R>: 
typedef struct {
    size_t executable_storage_q; // E - Executable Storage, където size(E) = q, по условие;
    Mode mode;                   // M - Mode;
    unsigned long pc;            // P - Program Counter;
    unsigned long reloc_l;       // R - Relocation base - l;
    unsigned long reloc_b;       // R - Relocation Bound - b;
    unsigned long registers[8];  // Няколко общи регистри;
} MachineState;

typedef enum {
    INST_NORMAL,
    INST_PRIVILEGED,        // При mode = 0 -> trap, иначе т.е. mode = 1 -> нормално изпълнение;
    INST_CONTROL_SENSITIVE, // При изпълнение НЕ прави trap, но променя control регистри и режима (reloc_l, reloc_b, mode);
    INST_ADDRESS_SENSITIVE, // При изпълнение НЕ прави trap, но поведението зависи от адреса (pc) спрямо reloc bounds;
    INST_BEHAVIOR_SENSITIVE // При изпълнение НЕ прави trap, но поведението зависи от местоположение/режим
} InstClass;

// i(S_1) = S_2:
typedef struct {
    const char *name;
    InstClass cls;
    void (*native_effect)(MachineState *); // Функция, която прилага ефекта на инструкцията върху машинното състояние
} Instruction;

// VMM/Hypervisor/CP = <D, A, v[i]> for i = [1,m]:
typedef struct {
    void (*dispatcher)(MachineState *, Instruction *, size_t); // При trap този модул решава какво да се прави по-нататък:
    void (*allocator)(MachineState *); // Решава как да се алокират ресурси за госта;
    void (*interpreter)(MachineState *, Instruction *); // За всяка priv. instr. симулира какво би направила реалната машина в контекста на VMM;
    size_t traps_handled;
} VMM;

void native_effect_noop(MachineState *m) { 
    m->pc += 1; 
}

void native_effect_inc_pc(MachineState *m) { 
    m->pc += 1; 
}

void native_effect_write_control(MachineState *m) { 
    m->reloc_l ^= 0xFF; // Примерна промяна на control регистър;
    m->pc += 1; 
}

void native_effect_mem_access(MachineState *m) {
    if (m->pc >= m->reloc_b) { // Ако pc е извън bounds, променяме регистър по различен начин
        m->registers[0] ^= 0xDEADBEEF;
    }
    else { 
        m->registers[0] += 1;
    }
    m->pc += 1;
}

bool is_privileged(Instruction *i) { 
    return i->cls == INST_PRIVILEGED; 
}

bool is_sensitive(Instruction *i) {
    return (i->cls == INST_CONTROL_SENSITIVE ||
            i->cls == INST_ADDRESS_SENSITIVE ||
            i->cls == INST_BEHAVIOR_SENSITIVE);
}
bool will_trap_on_user(Instruction *i) { 
    return is_privileged(i); 
}

void trap_handler(VMM *vmm, MachineState *guest, Instruction *inst) {
    printf("   [TRAP] Инструкция %s е прихваната от VMM!\n", inst->name);
    vmm->traps_handled++;
    vmm->interpreter(guest, inst); // Симулираме инструкцията в kernel контекст;
}

VMM GLOBAL_VMM;

void vmm_interpreter(MachineState *guest, Instruction *inst) {
    Mode old_mode = guest->mode;
    guest->mode = MODE_KERNEL;
    printf("   [VMM] Интерпретира %s в kernel mode.\n", inst->name);
    if (inst->native_effect) {
        inst->native_effect(guest);
    }
    guest->mode = old_mode;
}

void vmm_allocator(MachineState *guest) {
    // Маркираме, че VMM е коригиран за guest-а: примерно задаваме relocaion bounds: 
    guest->reloc_b = (unsigned long)(guest->executable_storage_q); 
}

// Последователно изпълнява n инструкции, прихваща trap-ове:
void vmm_dispatcher(MachineState *guest, Instruction *code, size_t n) { 
    for (size_t i = 0; i < n; ++i) {
        Instruction *inst = &code[i];
        printf("[EXEC] PC=%lu, MODE=%s, INST=%s\n",
               guest->pc, guest->mode == MODE_USER ? "USER" : "KERNEL", inst->name);

        if (guest->mode == MODE_USER && will_trap_on_user(inst)) {
            trap_handler(&GLOBAL_VMM, guest, inst);
        } else if (guest->mode == MODE_USER && is_sensitive(inst) && !will_trap_on_user(inst)) {
            printf("   [WARN] %s е чувствителна, но НЕ е privileged - VMM НЕ може да я прихване!\n", inst->name);
            if (inst->native_effect) inst->native_effect(guest);
        } else {
            if (inst->native_effect) inst->native_effect(guest);
        }
    }
}

// C_r: реалните контролни регистри (reloc_l, reloc_b, mode, etc.);
// C_v: виртуалните контролни регистри;

// За демонстрация ще поддържаме проста биекция: VMM съхранява "shadow"
// копия и при нужда ги връща. Операциите над C_v се симулират в C_r 
// (и обратно) чрез VMM.

typedef struct {
    unsigned long v_reloc_l;
    unsigned long v_reloc_b;
    Mode v_mode;
} ControlShadow;

ControlShadow map_control_regs(MachineState *real) {
    ControlShadow s = {real->reloc_l, real->reloc_b, real->mode};
    return s;
}

void unmap_control_regs(MachineState *real, ControlShadow *s) {
    // В реален VMM това би било по-сложно (контрол, права, проверка):
    real->reloc_l = s->v_reloc_l;
    real->reloc_b = s->v_reloc_b;
    real->mode = s->v_mode;
}
// Тест за еквивалентност:
bool run_native_execution(MachineState baseline, Instruction *code, size_t n, MachineState *out) {
    // Изпълняваме без VMM. Ако инструкцията е privileged и сме в user mode,
    // тя би предизвикала грешка на реалната машина - за симулацията правим:
    // return false, ако имаме привилеговано изпълнение в user mode;
    MachineState m = baseline;
    for (size_t i = 0; i < n; ++i) {
        Instruction *inst = &code[i];
        if (m.mode == MODE_USER && is_privileged(inst)) {
            printf("   [HW-TRAP] %s е привилегирована в user mode - реална машина ще прекъсне!\n", inst->name);
            return false;
        }
        if (inst->native_effect) inst->native_effect(&m);
    }
    *out = m;
    return true;
}

bool run_virtualized_execution(MachineState baseline, Instruction *code, size_t n, MachineState *out) {
    // Изпълняваме с VMM. VMM първо алокира ресурси, map-ва control regs
    // и стартира dispatcher. След това връща shadow обратно.
    MachineState m = baseline;
    GLOBAL_VMM.traps_handled = 0;
    GLOBAL_VMM.allocator(&m);
    ControlShadow shadow = map_control_regs(&m);
    GLOBAL_VMM.dispatcher(&m, code, n);
    unmap_control_regs(&m, &shadow);
    *out = m;
    return true;
}

int main(void) {
    GLOBAL_VMM.dispatcher = vmm_dispatcher;
    GLOBAL_VMM.allocator = vmm_allocator;
    GLOBAL_VMM.interpreter = vmm_interpreter;

    MachineState guest = {.executable_storage_q = 1024, 
        .mode = MODE_USER, 
        .pc = 0, 
        .reloc_l = 0, 
        .reloc_b = 512, 
        .registers = {0}};

    Instruction code1[] = {
        {"NOP", INST_NORMAL, native_effect_noop},
        {"READ_MEM", INST_ADDRESS_SENSITIVE, native_effect_mem_access},
        {"WRITE_CTRL", INST_CONTROL_SENSITIVE, native_effect_write_control},
        {"PRIV_OP", INST_PRIVILEGED, native_effect_inc_pc},
        {"BEHAV_S", INST_BEHAVIOR_SENSITIVE, native_effect_mem_access}
    };
    size_t n = sizeof(code1) / sizeof(code1[0]);

    printf("─────────────────────────────\nТЕСТ 1: Sensitive ⊄ Privileged\n─────────────────────────────\n");
    MachineState out_nat, out_virt;
    bool native_ok = run_native_execution(guest, code1, n, &out_nat);
    if (!native_ok)
        printf("Native execution failed: има privileged инструкция в user mode.\n");

    run_virtualized_execution(guest, code1, n, &out_virt);
    printf("Virtualized execution finished: PC=%lu, R0=%lu, traps=%zu\n",
           out_virt.pc, out_virt.registers[0], GLOBAL_VMM.traps_handled);

    // RESULT 1:
    // Тук някои чувствителни инструкции (READ_MEM, WRITE_CTRL, BEHAV_S);
    // НЕ са привилегировани -> VMM НЕ ги прихваща. Те променят състоянието директно;
    // Това нарушава теоремата на Popek & Goldberg;
    // Резултат: Виртуализацията НЕ е пълна - гостът може да промени ресурси.

    printf("\n─────────────────────────────\nТЕСТ 2: Sensitive ⊆ Privileged\n─────────────────────────────\n");
    Instruction code2[n];
    memcpy(code2, code1, sizeof(code1));
    for (size_t i = 0; i < n; ++i)
        if (is_sensitive(&code2[i])) code2[i].cls = INST_PRIVILEGED;

    native_ok = run_native_execution(guest, code2, n, &out_nat);
    if (!native_ok)
        printf("Native execution blocked: privileged ops в user mode - trap.\n");

    run_virtualized_execution(guest, code2, n, &out_virt);
    printf("Virtualized execution finished: PC=%lu, R0=%lu, traps=%zu\n",
           out_virt.pc, out_virt.registers[0], GLOBAL_VMM.traps_handled);

    // RESULT 2:
    // Всички чувствителни инструкции са вече privileged;
    // При изпълнение в user mode -> trap -> VMM ги прихваща и интерпретира;
    // Така VMM контролира ресурсите, изпълнението е еквивалентно и е ефективно;
    // Резултат: Виртуализацията е успешна.

    printf("\n─────────────────────────────\nТЕСТ 3: Guest в SUPERVISOR режим\n─────────────────────────────\n");
    MachineState guest_kernel = guest;
    guest_kernel.mode = MODE_KERNEL;

    bool native_ok3 = run_native_execution(guest_kernel, code1, n, &out_nat);
    if (!native_ok3) {
        printf("[UNEXPECTED] native failed in kernel mode.\n");
    } else {
        printf("Native (supervisor) finished: PC=%lu, R0=%lu\n", out_nat.pc, out_nat.registers[0]);
    }

    run_virtualized_execution(guest_kernel, code1, n, &out_virt);
    printf("Virtualized (guest in supervisor) finished: PC=%lu, R0=%lu, traps=%zu\n",
           out_virt.pc, out_virt.registers[0], GLOBAL_VMM.traps_handled);

    // RESULT 3:
    // Гостът е в SUPERVISOR режим -> привилегированите инструкции НЕ предизвикват trap при current dispatcher;
    // Следователно VMM НЕ прихваща привилегировани инструкции (traps_handled = 0;
    // Ако гост работи в supervisor mode директно, VMM няма да може да прихване тези операции без допълнителни механизми:
    // - ring deprivileging, binary translation или хардуерно поддържани виртуализационни инструкции;
    // Резултат: Виртуализацията, в нашия случай, НЕ е пълна, VMM не може да контролира привилегированите операции.

    return 0;
}
