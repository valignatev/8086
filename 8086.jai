// RTFM. Starting from page 160
// https://edge.edx.org/c4x/BITSPilani/EEE231/asset/8086_family_Users_Manual_1_.pdf

// NOTE: this leaks all the memory in the world, but maybe I don't care
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
        decoding_instruction := decoding_table.instructions[current_byte];

        if decoding_instruction {
            result_chunk, decoding_cursor = decoding_instruction(bytes, decoding_cursor);
            print_to_builder(*result_builder, "%\n", result_chunk);
        } else {
            print("No decoding instruction for byte %\n", formatInt(current_byte, base=16));
            exit(1);
        }
        if decoding_cursor == cast(u64)bytes.count decoding_done = true;
    }
    print(builder_to_string(*result_builder));
}

// Machine Instruction Decoding Guide. Intel manual defines 255 instruction formats
// First first byte is how you know what instruction to decode
Decode_Instruction_Proc :: #type (bytes: []u8, cursor: u64) -> string, u64;

// MOV register/memory to/from register
mov_register_memory_to_from_register :: (bytes: []u8, cursor: u64) -> string, u64 {
    reference_opcode := 0b100010;

    new_cursor := cursor;

    // First byte is the opcode, the D and the W fields
    current_byte := bytes[new_cursor];
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
    current_byte = bytes[new_cursor];

    // first 2 bits of the second byte - the MOD (mode) field
    mode := get_first_n_bits(current_byte, 2);
    //print("the MOD field is %\n", formatInt(mode, base=2, minimum_digits=2));

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
    reg_mem_mnemonic := "UNKNOWN";
    if mode == 0b11 {
        reg_mem_mnemonic = REG_ENCODING[reg_memw];
    }
    // print("R/M mnemonic is %\n", reg_mem_mnemonic);

    if direction {
        print_to_builder(*builder, "%, %", reg_mnemonic, reg_mem_mnemonic);
    } else {
        print_to_builder(*builder, "%, %", reg_mem_mnemonic, reg_mnemonic);
    }

    new_cursor += 1;
    return builder_to_string(*builder), new_cursor;
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

// First 3 bits of the index is REG field, 4th is W field
// This encoding table is for REG (Register) field, and for R/M (Register/Memory)
// field when MOD (Mode) field is 11 (see tables 4-9 and 4-10 in the manual)
REG_ENCODING : [16]string : .[
  
//  0000  0001  0010  0011  0100  0101  0110  0111  1000  1001  1010  1011
    "al", "ax", "cl", "cx", "dl", "dx", "bl", "bx", "ah", "sp", "ch", "bp",
//  1100  1101  1110  1111
    "dh", "si", "bh", "di",
];


// Loose data assignments are not currently supported at global scope, only
// inside structs
Table_4_13 :: struct {
    instructions: [256]Decode_Instruction_Proc;

    instructions[0x88] = mov_register_memory_to_from_register;
    instructions[0x89] = mov_register_memory_to_from_register;
    instructions[0x8A] = mov_register_memory_to_from_register;
    instructions[0x8B] = mov_register_memory_to_from_register;
}


#import "Basic";
#import "File";
