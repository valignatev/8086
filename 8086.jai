// RTFM. Starting from page 160
// https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf

// NOTE: this leaks all the memory in the world, but maybe I don't care
// NOTE: I'm directly composing strings here and create a lot of small string builders
// It's probably better to use a single builder that I just pass around, or to have an
// intermetiate representation for disassembled code that I later print to builder
// in a separate step. For now, I keep it stupid and inefficient to see where it goes.
// How to test: compile .asm files with NASM and then provide them to this program
// then compare the output to the asm source code. I'll make a proper testing harness
// in the future maybe.

main :: () {
    args := get_command_line_arguments();

    if args.count < 2 {
        print("assembly executable required as a parameter\n");
        exit(1);
    }

    executable := args[1];
    print("Disassembling executable %\n", executable);
    bytes := cast([]u8)read_entire_file(executable);

    decoding_table: Table_4_13;

    decoding_cursor : u64 = 0;
    decoding_done := false;

    result_chunk := "";
    result_builder: String_Builder;
    print_to_builder(*result_builder, "bits 16\n\n");
    while !decoding_done {
        current_byte := bytes[decoding_cursor];
        // Instead of testing for opcode prefixes of a different length
        // and iffing on them, we use the fact that the Manual has a table 4-13
        // that you can use to dispatch between different decoding routines based
        // on the first byte of the instruction
        decoding_instruction := decoding_table.instructions[current_byte];

        if decoding_instruction {
            result_chunk, decoding_cursor = decoding_instruction(bytes, decoding_cursor);
            print_to_builder(*result_builder, "%\n", result_chunk);
        } else {
            print(
                "No decoding instruction for byte %, currently disassembled code is:\n",
                formatInt(current_byte, base=16),
            );
            print(builder_to_string(*result_builder));
            exit(1);
        }
        if decoding_cursor == cast(u64)bytes.count decoding_done = true;
    }
    print(builder_to_string(*result_builder));
}

// MOV register/memory to/from register
mov_register_memory_to_from_register :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b100010;

    new_cursor := cursor;

    // First byte is the opcode, the D and the W fields
    current_byte := bytes[new_cursor];
    // print("\nCurrent byte is %\n", formatInt(current_byte, base=2, minimum_digits=8));

    // First 6 bits is an opcode
    opcode := get_first_n_bits(current_byte, 6);
    //print("Opcode is %\n", formatInt(opcode, base=2, minimum_digits=6));

    assert(
        opcode == reference_opcode,
        tprint(
            "Opcode % doesn't match reference opcode %",
            formatInt(opcode, base=2),
            formatInt(reference_opcode, base=2),
        ),
    );

    builder: String_Builder;
    print_to_builder(*builder, "mov ");

    // 7th bit is a direction (the D field)
    direction := get_nth_bit(current_byte, 7);
    //print("the D field is %\n", formatInt(direction, base=2));

    // 8th bit is a "wide" (the W field)
    wide := get_nth_bit(current_byte, 8);
    //print("the W field is %\n", formatInt(wide, base=2));

    new_cursor += 1;

    // Second byte is MOD, REG and R/M fields
    current_byte = bytes[new_cursor];
    // print("Current byte is %\n", formatInt(current_byte, base=2, minimum_digits=8));

    // first 2 bits of the second byte - the MOD (mode) field
    mode := get_first_n_bits(current_byte, 2);
    // print("the MOD field is %\n", formatInt(mode, base=2, minimum_digits=2));

    // bits 3-5 of the second byte - the REG (register) field
    register := get_n_bits(current_byte, 3, 3);
    //print("the REG field is %\n", formatInt(register, base=2, minimum_digits=3));

    // bits 6-8 of the second byte - the R/M (register/memory) field
    reg_mem := get_n_bits(current_byte, 6, 3);
    //print("the R/M field is %\n", formatInt(reg_mem, base=2, minimum_digits=3));

    regw := (register << 1) + wide;
    reg_mnemonic := REG_ENCODING[regw];
    //print("REG mnemonic is %\n", reg_mnemonic);

    reg_memw := (reg_mem << 1) + wide;
    rm_mnemonic := "UNKNOWN";
    if mode == 0b11 {
        rm_mnemonic = REG_ENCODING[reg_memw];
    } else {
        // Handle the DIRECT ADDRESS special case
        if mode == 0b00 && reg_mem == 0b110 {
            new_cursor += 2;
            direct_address := ((cast(u16)bytes[new_cursor]) << 8) + bytes[new_cursor-1];
            rm_mnemonic = sprint("[%]", direct_address);
        } else {
            rm_address := RM_EFFECTIVE_ADDRESS_CALCULATION[reg_mem];
            if mode == 0b00 {
                rm_mnemonic = sprint("[%]", rm_address);
            } else {
                // Else we have at least a single byte of displacement
                new_cursor += 1;
                current_byte = bytes[new_cursor];
                disp_lo: u8 = current_byte;
                disp_hi: u8;
                disp_value: u16;

                if mode == 0b10 {
                    new_cursor += 1;
                    current_byte = bytes[new_cursor];
                    disp_hi = current_byte;
                }
                disp_value = ((cast(u16)disp_hi) << 8) + disp_lo;
                if disp_value == 0 {
                    rm_mnemonic = sprint("[%]", rm_address);
                } else {
                    rm_mnemonic = sprint("[% + %]", rm_address, disp_value);
                }
            }
        }
    }
    // print("R/M mnemonic is %\n", rm_mnemonic);

    if direction {
        print_to_builder(*builder, "%, %", reg_mnemonic, rm_mnemonic);
    } else {
        print_to_builder(*builder, "%, %", rm_mnemonic, reg_mnemonic);
    }

    return builder_to_string(*builder), new_cursor + 1;
}


move_immediate_to_register :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b1011;

    new_cursor := cursor;

    // First byte is the opcode, W field and the REG field
    current_byte := bytes[new_cursor];
    /* print("\nCurrent byte is %\n", formatInt(current_byte, base=2)); */

    // First 4 bits is the opcode
    opcode := get_first_n_bits(current_byte, 4);

    assert(
        opcode == reference_opcode,
        tprint(
            "Opcode % doesn't match reference opcode %",
            formatInt(opcode, base=2),
            formatInt(reference_opcode, base=2),
        ),
    );

    builder: String_Builder;
    print_to_builder(*builder, "mov ");

    // 5th bit is the W field
    wide := get_nth_bit(current_byte, 5);
    /* print("W field is %\n", formatInt(wide, base=2)); */

    // 6-8th bits is the REG field
    register := get_n_bits(current_byte, 6, 3);
    /* print("REG field is %\n", formatInt(register, base=2, minimum_digits=3)); */
    print_to_builder(*builder, "%, ",  REG_ENCODING[(register << 1) + wide]);

    new_cursor += 1;
    current_byte = bytes[new_cursor];

    // 2nd byte is DATA-LO
    data_lo := current_byte;
    /* print("DATA-LO is %\n", formatInt(current_byte, base=2)); */

    result: u16;
    if wide == 1 {
        // if W field is 1, 3rd byte is DATA-HI
        new_cursor += 1;
        current_byte = bytes[new_cursor];
        /* print("DATA-HI is %\n", formatInt(current_byte, base=2)); */
        data_hi := current_byte;
        result = ((cast(u16) data_hi) << 8) + data_lo;
    } else {
        result = data_lo;
    }
    print_to_builder(*builder, "%", result);

    return builder_to_string(*builder), new_cursor + 1;
}


move_immediate_to_register_memory :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b1100011;
    new_cursor := cursor;

    // First byte is 7 bits for the opcode, and 1 bit for the W field
    current_byte := bytes[new_cursor];
    /* print("\nCurrent byte is %\n", formatInt(current_byte, base=2, minimum_digits=8)); */
    opcode := get_first_n_bits(current_byte, 7);
    assert(
        opcode == reference_opcode,
        tprint(
            "Opcode % doesn't match reference opcode %",
            formatInt(opcode, base=2),
            formatInt(reference_opcode, base=2),
        ),
    );

    builder: String_Builder;
    print_to_builder(*builder, "mov ");

    // W field is the 8th bit of the first byte
    wide := get_nth_bit(current_byte, 8);

    new_cursor += 1;
    current_byte = bytes[new_cursor];
    /* print("Current byte is %\n", formatInt(current_byte, base=2, minimum_digits=8)); */

    // MOD, REG and R/M are the same as in register to register move,
    // but REG is always 0
    mode := get_first_n_bits(current_byte, 2);
    register := get_n_bits(current_byte, 3, 3);
    assert(register == 0, "REG field should always be 0 in this instruction");
    reg_mem := get_n_bits(current_byte, 6, 3);

    // R/M mnemonic copy-paste. Will deduplicate later when I see that the pattern
    // definitely applies. For now it seems like it does, but I'll let it cook
    reg_memw := (reg_mem << 1) + wide;
    rm_mnemonic := "UNKNOWN";
    if mode == 0b11 {
        rm_mnemonic = REG_ENCODING[reg_memw];
    } else {
        // Handle the DIRECT ADDRESS special case, aka 16-bit displacement
        if mode == 0b00 && reg_mem == 0b110 {
            new_cursor += 2;
            direct_address := ((cast(u16)bytes[new_cursor]) << 8) + bytes[new_cursor-1];
            rm_mnemonic = sprint("[%]", direct_address);
        } else {
            rm_address := RM_EFFECTIVE_ADDRESS_CALCULATION[reg_mem];
            if mode == 0b00 {
                rm_mnemonic = sprint("[%]", rm_address);
            } else {
                // Else we have at least a single byte of displacement
                new_cursor += 1;
                current_byte = bytes[new_cursor];
                disp_lo: u8 = current_byte;
                disp_hi: u8;
                disp_value: u16;

                if mode == 0b10 {
                    new_cursor += 1;
                    current_byte = bytes[new_cursor];
                    disp_hi = current_byte;
                }
                disp_value = ((cast(u16)disp_hi) << 8) + disp_lo;
                if disp_value == 0 {
                    rm_mnemonic = sprint("[%]", rm_address);
                } else {
                    rm_mnemonic = sprint("[% + %]", rm_address, disp_value);
                }
            }
        }
    }

    // Handle data bytes next
    new_cursor += 1;
    data_lo := bytes[new_cursor];
    /* print("DATA-LO is %\n", data_lo); */

    data_hi: u8;
    value: u16;
    size := "byte";
    if wide == 1 {
        new_cursor += 1;
        data_hi = bytes[new_cursor];
        size = "word";
    }
    value = ((cast(u16)data_hi) << 8) + data_lo;

    print_to_builder(*builder, "%, % %", rm_mnemonic, size, value);

    return builder_to_string(*builder), new_cursor + 1; 
}


// We could've handled both from acc to mem and from mem to acc in the same
// procedure, but I'm just being consistent with the Manual - it's shown as two
// different instructions there even though they're only different in their D field
mov_memory_to_accumulator :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b1010000;
    new_cursor := cursor;
    current_byte := bytes[new_cursor];
    opcode := get_first_n_bits(current_byte, 7);
    assert(
        opcode == reference_opcode,
        tprint(
            "Opcode % doesn't match reference opcode %",
            formatInt(opcode, base=2),
            formatInt(reference_opcode, base=2),
        ),
    );

    builder: String_Builder;
    print_to_builder(*builder, "mov ");

    // W field is 8th bit of the first byte
    wide := get_nth_bit(current_byte, 8);

    new_cursor += 1;
    addr_lo := bytes[new_cursor];
    addr_hi: u8;
    address: u16;

    accumulator := "al";
    if wide {
        new_cursor += 1;
        addr_hi = bytes[new_cursor];
        accumulator = "ax";
    }
    address = ((cast(u16)addr_hi) << 8) + addr_lo;

    print_to_builder(*builder, "%, [%]", accumulator, address);
    return builder_to_string(*builder), new_cursor + 1;
}


mov_accumulator_to_memory :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b1010001;
    new_cursor := cursor;
    current_byte := bytes[new_cursor];
    opcode := get_first_n_bits(current_byte, 7);
    assert(
        opcode == reference_opcode,
        tprint(
            "Opcode % doesn't match reference opcode %",
            formatInt(opcode, base=2),
            formatInt(reference_opcode, base=2),
        ),
    );

    builder: String_Builder;
    print_to_builder(*builder, "mov ");

    // W field is 8th bit of the first byte
    wide := get_nth_bit(current_byte, 8);

    new_cursor += 1;
    addr_lo := bytes[new_cursor];
    addr_hi: u8;
    address: u16;

    accumulator := "al";
    if wide {
        new_cursor += 1;
        addr_hi = bytes[new_cursor];
        accumulator = "ax";
    }
    address = ((cast(u16)addr_hi) << 8) + addr_lo;

    print_to_builder(*builder, "[%], %", address, accumulator);
    return builder_to_string(*builder), new_cursor + 1;
}

/* We count bits from left to right in these helper functions (so from MSB to LSB).
That's why you see 8 minus something, just to adjust to this convention.
I could've counted bits from LSB to MSB, but I just want to be consistent
with how Intel 8086 manual talks about the bits in the encoding section.
*/

// Get n bits of a byte, from start inclusive
// TODO: no error checking
get_n_bits :: (byte: u8, start: u8, n: u8) -> u8 {
    // 8 is a magic number - number of bits in a byte
    // end is the the position of the last bit that we need.
    // For example, if we start at 1 and we need 6 bits, then we get first
    // 1, 2, 3, 4, 5, 6 bits - the end is 6. So start and end are both inclusive.
    // if we start at 3 and get 3 bits, we get 3, 4, 5th bits - the end is 5;
    end := start + n - 1;

    // Example: if we need 3 bits, 3 to 5, then the mask will be 0b00111000
    mask := cast(u8)((1 << n) - 1) << (8 - end);

    return (byte & mask) >> (8 - end);
}


// Get nth bit of a byte
get_nth_bit :: (byte: u8, n: u8) -> u8 {
    return inline get_n_bits(byte, n, 1);
}


get_first_n_bits :: (byte: u8, n: u8) -> u8 {
    return inline get_n_bits(byte, 1, n);
}


// This encoding table is for REG (Register) field, and for R/M (Register/Memory)
// field when MOD (Mode) field is 11 (see tables 4-9 and 4-10 in the manual)
// First 3 bits of the index is REG field, 4th is W field
REG_ENCODING : [16]string : .[
//  0000  0001  0010  0011  0100  0101  0110  0111  1000  1001  1010  1011
    "al", "ax", "cl", "cx", "dl", "dx", "bl", "bx", "ah", "sp", "ch", "bp",
//  1100  1101  1110  1111
    "dh", "si", "bh", "di",
];


/* Second part of Table 4-10: values of R/M field when MOD is something else than 11
Bits of the index are the value of R/M field. We don't hardcode the entire table
here as R/M values are the same but with a different displacement based on the
MOD field with only a single exception that we also handle separately. */
RM_EFFECTIVE_ADDRESS_CALCULATION : [8]string : .[
/*  MOD 00, R/M 110 is an exception, it contains DIRECT ADDRESS instead of
    effective address calculation                           !!!
                                                            !!!
    000        001        010        011        100   101   110   111 */
    "bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx",
];


// Machine Instruction Decoding Guide. Intel manual defines 255 instruction formats
// First byte is how you know what instruction to decode
Decode_Instruction_Proc :: #type (bytes: []u8, cursor: u64) -> string, u64;

// Loose data assignments are not currently supported at global scope, only
// inside structs
Table_4_13 :: struct {
    instructions: [256]Decode_Instruction_Proc;
    // TODO: instead of providing same functions, we can bake some of known
    //  invariants of the first byte that are guaranteed, instead of calculating
    //  them at runtime. for example, we can bake the register into
    //  move_immediate_to_register variants.
    instructions[0x88] = mov_register_memory_to_from_register;
    instructions[0x89] = mov_register_memory_to_from_register;
    instructions[0x8A] = mov_register_memory_to_from_register;
    instructions[0xA0] = mov_memory_to_accumulator;
    instructions[0xA1] = mov_memory_to_accumulator;
    instructions[0xA2] = mov_accumulator_to_memory;
    instructions[0xA3] = mov_accumulator_to_memory;
    instructions[0x8B] = mov_register_memory_to_from_register;
    instructions[0xB8] = move_immediate_to_register;
    instructions[0xB9] = move_immediate_to_register;
    instructions[0xB9] = move_immediate_to_register;
    instructions[0xBA] = move_immediate_to_register;
    instructions[0xBB] = move_immediate_to_register;
    instructions[0xBC] = move_immediate_to_register;
    instructions[0xBD] = move_immediate_to_register;
    instructions[0xBE] = move_immediate_to_register;
    instructions[0xBF] = move_immediate_to_register;
    instructions[0xC6] = move_immediate_to_register_memory;
    instructions[0xC7] = move_immediate_to_register_memory;
}


#import "Basic";
#import "File";
