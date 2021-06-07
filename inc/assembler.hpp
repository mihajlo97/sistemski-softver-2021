#ifndef _ASSEMBLER_H_
#include <string>
#include <vector>
#include <map>
#include <limits>
#include <iostream>
#include <fstream>
#define _ASSEMBLER_H_

typedef unsigned char byte_t;
typedef short int word_t;

namespace ASM
{
    /* 
        Instruction format: [ INSTR_CODE | REG_DST..REG_SRC | ADDR_MODE | DATA_HIGH | DATA_LOW ]
            1st byte: Instruction code
            2nd byte: 7..4 bits destination register code, 3..0 bits source register code
            3rd byte: addressing modes, 7..4 bits reserved only for registry indirect addressing
            4th & 5th byte: instruction operands
        Instruction POP and PUSH have their instructions in assembly but are not supported by the provided CPU architecture.
        This means the assembler will have to parse these instructions into an array of other instructions.
        They do not have their own bytecode.
        All data in memory is represented with MAX_DATA_WIDTH bits.
    */

    typedef unsigned char byte_t;
    typedef short int word_t;

    const word_t MAX_DATA_WIDTH = 16;

    enum Instr
    {
        INVALID_INSTR = -1,
        HALT = 0, /* halt processor */
        INT,      /* perform software interrupt */
        IRET,     /* return from interrupt routine (updates PSW) */
        CALL,     /* call function */
        RET,      /* return from function */
        JMP,      /* perform jump */
        JEQ,      /* perform jump when equal */
        JNE,      /* perform jump when not equal */
        JGT,      /* perform jump when greater than */
        XCHG,     /* exchange two values */
        ADD,      /* perform addition */
        SUB,      /* perform subtraction */
        MUL,      /* perform multiplication */
        DIV,      /* perform division */
        CMP,      /* compare values (updates NCOZ flags in PSW) */
        NOT,      /* perform bitwise not */
        AND,      /* perform bitwise and */
        OR,       /* perform bitwise or */
        XOR,      /* perform bitwise xor */
        TEST,     /* perform bitwise and (updates N and Z flags in PSW) */
        SHL,      /* perform bitwise shift left (updates N, C, Z flags in PSW) */
        SHR,      /* perform bitwise shift right (updates N, C, Z flags in PSW) */
        LDR,      /* load value */
        STR,      /* store value */
        PUSH,     /* pushes operand to stack (not directly supported by CPU, needs transpiling) */
        POP       /* pops operand from stack (not directly supported by CPU, needs transpiling) */
    };

    const byte_t InstrCode[] = {
        0x00, /* HALT */
        0x10, /* INT */
        0x20, /* IRET */
        0x30, /* CALL */
        0x40, /* RET */
        0x50, /* JMP */
        0x51, /* JEQ */
        0x52, /* JNE */
        0x53, /* JGT */
        0x60, /* XCHG */
        0x70, /* ADD */
        0x71, /* SUB */
        0x72, /* MUL */
        0x73, /* DIV */
        0x74, /* CMP */
        0x80, /* NOT */
        0x81, /* AND */
        0x82, /* OR */
        0x83, /* XOR */
        0x84, /* TEST */
        0x90, /* SHL */
        0x91, /* SHR */
        0xA0, /* LDR */
        0xB0  /* STR */
    };

    const std::string Instruction[] = {
        "halt",
        "int",
        "iret",
        "call",
        "ret",
        "jmp",
        "jeq",
        "jne",
        "jgt",
        "xchg",
        "add",
        "sub",
        "mul",
        "div",
        "cmp",
        "not",
        "and",
        "or",
        "xor",
        "test",
        "shl",
        "shr",
        "ldr",
        "str",
        "push",
        "pop" /*  */
    };

    /*
        Describes the instruction set for this assembly language.
        Instructions only have registry direct addressing mode enabled except for the following:
            str, ldr, jmp, jeq, jne, jgt, call;
    */

    enum InstrSize
    {
        BYTE = 1,
        WORD = 2,
        STANDARD = 3,
        QUAD = 4,
        MAX = 5,
        NONE = -1
    };

    enum InstrOperand
    {
        NO_OPERANDS = 0,
        SINGLE = 1,
        DOUBLE = 2
    };

    enum InstrType
    {
        NOT_JUMP = 0,
        JUMP = 1
    };

    typedef struct InstrDescTable
    {
        Instr instr;
        InstrOperand operandCount;
        InstrType type;
        byte_t code;
        InstrSize size;
        InstrSize altSize;
        bool allowOtherAddrModes;
    } InstrDescTable_t;

    const InstrDescTable_t InstructionDescTable[] = {
        {Instr::HALT, InstrOperand::NO_OPERANDS, InstrType::NOT_JUMP, InstrCode[Instr::HALT], InstrSize::BYTE, InstrSize::NONE, false},
        {Instr::INT, InstrOperand::SINGLE, InstrType::NOT_JUMP, InstrCode[Instr::INT], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::IRET, InstrOperand::NO_OPERANDS, InstrType::NOT_JUMP, InstrCode[Instr::IRET], InstrSize::BYTE, InstrSize::NONE, false},
        {Instr::CALL, InstrOperand::SINGLE, InstrType::JUMP, InstrCode[Instr::CALL], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::RET, InstrOperand::NO_OPERANDS, InstrType::NOT_JUMP, InstrCode[Instr::RET], InstrSize::BYTE, InstrSize::NONE, false},
        {Instr::JMP, InstrOperand::SINGLE, InstrType::JUMP, InstrCode[Instr::JMP], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::JEQ, InstrOperand::SINGLE, InstrType::JUMP, InstrCode[Instr::JEQ], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::JNE, InstrOperand::SINGLE, InstrType::JUMP, InstrCode[Instr::JNE], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::JGT, InstrOperand::SINGLE, InstrType::JUMP, InstrCode[Instr::JGT], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::XCHG, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::XCHG], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::ADD, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::ADD], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::SUB, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::SUB], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::MUL, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::MUL], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::DIV, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::DIV], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::CMP, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::CMP], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::NOT, InstrOperand::SINGLE, InstrType::NOT_JUMP, InstrCode[Instr::NOT], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::AND, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::AND], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::OR, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::OR], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::XOR, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::XOR], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::TEST, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::TEST], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::SHL, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::SHL], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::SHR, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::SHR], InstrSize::WORD, InstrSize::NONE, false},
        {Instr::LDR, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::LDR], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::STR, InstrOperand::DOUBLE, InstrType::NOT_JUMP, InstrCode[Instr::STR], InstrSize::STANDARD, InstrSize::MAX, true},
        {Instr::PUSH, InstrOperand::SINGLE, InstrType::NOT_JUMP, InstrCode[Instr::PUSH], InstrSize::MAX, InstrSize::NONE, false},
        {Instr::POP, InstrOperand::SINGLE, InstrType::NOT_JUMP, InstrCode[Instr::POP], InstrSize::MAX, InstrSize::NONE, false} /* */
    };

    /*
        Registers r0..r5 are general purpose registers. 
        r6 is used as stack pointer (sp). r7 is used as program counter (pc).
        PSW is the program status word register.
        All registers are 16 bit (2 bytes, 1 word).
        r7 (pc) always holds address of next instruction to be executed.
        r6 (sp) always points to address of the data at the top of the stack.
        The sp value is decreased by 16 bits when using push and increased by 16 bits when using pop.
        PSW bit format: [15..13 interrupt flags, 12..4 not in use, 3..0 instruction flags]
        Instruction flags:  3 - N (negative, data represents a negative number),
                            2 - C (carry, unsigned overflow),
                            1 - O (overflow, signed overflow),
                            0 - Z (zero, data represents the number 0)
    */

    enum Regs
    {
        R0 = 0,
        R1 = 2,
        R2 = 4,
        R3 = 6,
        R4 = 8,
        R5 = 10,
        R6 = 12,
        R7 = 16,
        SP = 12,
        PC = 16,
        PSW = 20
    };

    const byte_t RegCode[] = {
        0x00, /* r0 */
        0x00, /* R0 (same register as r0, just an uppercase symbol reference) */
        0x01, /* r1 */
        0x01, /* R1 */
        0x02, /* r2 */
        0x02, /* R2 */
        0x03, /* r3 */
        0x03, /* R3 */
        0x04, /* r4 */
        0x04, /* R4 */
        0x05, /* r5 */
        0x05, /* R5 */
        0x06, /* r6 */
        0x06, /* R6 */
        0x06, /* sp == r6 */
        0x06, /* SP == R6 */
        0x07, /* r7 */
        0x07, /* R7 */
        0x07, /* pc == r7 */
        0x07, /* PC == R7 */
        0x08, /* psw */
        0x08  /* PSW */
    };

    const std::string Register[] = {
        "r0",
        "R0",
        "r1",
        "R1",
        "r2",
        "R2",
        "r3",
        "R3",
        "r4",
        "R4",
        "r5",
        "R5",
        "r6",
        "R6",
        "sp",
        "SP",
        "r7",
        "R7",
        "pc",
        "PC",
        "psw",
        "PSW" /* */
    };

    /*
        Most instructions can only use registry direct addressing mode, while the rest are reserved for jumps and such.
        In the following, operand may refer to the value used by the instruction or to a destination address for jumps.
        DATA will refer to the instruction bytes DATA_HIGH and DATA_LOW as a single 16 bit signed value.
        REG will refer to the value held by a register referred to by the REG_SRC bits in the instruction.
        MEM will refer to the contents of the memory module as a continous array.  
        The instrtuction has the following addressing modes available:
            Immediate - operand = DATA;
            Registry direct - operand = REG;
            Registry indirect - operand = MEM[REG];
            Registry indirect with offset - operand = MEM[REG + DATA];
            Memory direct - operand = MEM[DATA];
        When using the registry indirect modes, some side effects can occur.
        These side effects are decided using the 7..4 bits inside the ADDR_MODE byte in the instruction.
        The side effects are as following:
            Decrease or increase register value by 2 before evaluating the operand value.
            Decrease or increase register value by 2 after evaluating the operand value.
        These side effects will be applied to the register denoted by the REG_SRC bits in the instruction.
        The side effects are only applied when explicitly stated by the programmer.
    */

    enum AddrMode
    {
        IMM = 0,
        REG_DIR,
        REG_INDIR,
        REG_INDIR_W_OFF,
        MEM_DIR,
        REG_DIR_W_OFF,
        REG_INDIR_DEC_BEFORE,
        REG_INDIR_DEC_BEFORE_W_OFF,
        REG_INDIR_INC_BEFORE,
        REG_INDIR_INC_BEFORE_W_OFF,
        REG_INDIR_DEC_AFTER,
        REG_INDIR_DEC_AFTER_W_OFF,
        REG_INDIR_INC_AFTER,
        REG_INDIR_INC_AFTER_W_OFF,
    };

    const byte_t AddrModeCode[] = {
        0x00, /* immediate */
        0x01, /* registry direct */
        0x02, /* registry indirect, no offset */
        0x03, /* registry indirect, 16 bit signed offset */
        0x04, /* memory direct */
        0x05, /* registry direct with 16 bit signed offset */
        0x12, /* reg. indir. no offset, reg - 2 before addr eval */
        0x13, /* reg. indir. with offset, reg - 2 before addr eval */
        0x22, /* reg. indir. no offset, reg + 2 before addr eval */
        0x23, /* reg. indir. with offset, reg + 2 before addr eval */
        0x32, /* reg. indir. no offset, reg - 2 after addr eval */
        0x33, /* reg. indir. with offset, reg - 2 after addr eval */
        0x42, /* reg. indir. no offset, reg + 2 after addr eval */
        0x43, /* reg. indir. with offset, reg + 2 after addr eval */
    };

    /*
        Allowed characters for identifiers: a-zA-Z0-9_
        Identifiers cannot start with numbers.
        The rest of the reserved symbols used in the assembly language are denoted below.
    */

    enum Symbol
    {
        COMMA = 0,
        DOT,
        COLON,
        HASHTAG,
        DOLLAR,
        PERCENT,
        BRACKET_OPEN,
        BRACKET_CLOSE,
        STAR,
        PLUS,
        MINUS,
        APOSTROPHE
    };

    const byte_t ReservedSymbols[] = {
        ',',  /* general purpose */
        '.',  /* declare directive */
        ':',  /* declare label */
        '#',  /* begin comment */
        '$',  /* immediate value */
        '%',  /* use pc relative addressing */
        '[',  /* begin indirect addressing */
        ']',  /* end indirect addressing */
        '*',  /* denotes address jump */
        '+',  /* address addition */
        '-',  /* represent negative integer */
        '\'', /* wrap around char literals */
    };

    /*
        There are several available directives for use in the language denoted bellow.
        All directives start with the '.' symbol.
    */

    enum Directive
    {
        DOT_GLOBAL = 0,
        DOT_EXTERN,
        DOT_SECTION,
        DOT_WORD,
        DOT_SKIP,
        DOT_EQU,
        DOT_END,
        INVALID_DIRECTIVE = -1
    };

    const std::string Directives[] = {
        ".global",  /* exports global symbols */
        ".extern",  /* declares external symbols */
        ".section", /* denotes a section in the program */
        ".word",    /* reserves 16 bits for symbols or literals */
        ".skip",    /* reserves number of bytes initialized with 0 as specified by literal */
        ".equ",     /* asigns literal to symbol */
        ".end"      /* denotes end of program */
    };
}

namespace Assembler
{
    typedef std::string token_t;
    typedef std::string section_name_t;
    typedef std::string symbol_name_t;
    typedef std::vector<std::string> token_container_t;

    /* 
        Error handling during assembler execution.
    */

    enum ErrorCode
    {
        BAD_ARGS = 0,
        FILE_NOT_FOUND,
        UNKNOWN_INSTRUCTION,
        UNSUPPORTED_OPERANDS,
        NO_SECTION,
        INVALID_IDENTIFIER,
        DUPLICATE_IDENTIFIER,
        UNKNOWN_DIRECTIVE,
        INVALID_LITERAL,
        INVALID_INITIALIZER,
        MULTIPLE_LABELS,
        REDECLARED_EXTERN,
        UNDECLARED_GLOBAL,
        NO_END_DIRECTIVE /* */
    };

    enum WarningCode
    {
        TRUNCATE = 0,
        IGNORE_AFTER_END
    };

    std::string ErrorMessages[] = {
        "Error: Incorrect assembler call. Assembler usage: \nasembler [-o <output_file_name>] <input_file_name.s>",
        "Error: Failed to open input file. Check if the file is in the current directory.",
        "Error: Unknown instruction referenced.",
        "Error: This instruction does not support the number of operands specified.",
        "Error: No section declared before this line. All labels and instructions must be preceeded by a section declaration.",
        "Error: Invalid or missing symbol name in line.",
        "Error: Symbol name already declared.",
        "Error: Unknown directive referenced.",
        "Error: Invalid literal declared.",
        "Error: Invalid initializer declared.",
        "Error: Cannot declare multiple labels in line.",
        "Error: Cannot redeclare an external symbol.",
        "Error: Cannot declare symbol as global. Symbol declaration missing.",
        "Error: Program must contain a .end directive." /* */
    };

    std::string WarningMessages[] = {
        "Warning: Truncating literal to 16 bits.",
        "Warning: Lines declared after the .end directive will be ignored." /* */
    };

    void assertCorrectProgramCall(int argc, char *argv[]);          /* check if user called program incorrectly */
    void reportWarning(int lineNumber, WarningCode code);           /* issue warning to user during assembling */
    void reportErrorAndExit(ErrorCode code);                        /* report error message and exit with error code */
    void reportErrorAtLineAndThrow(int lineNumber, ErrorCode code); /* reports at which line in the source file the error occurred and exits */
    void willThrowOnToken(token_t &token);                          /* reports which token caused an exception throw */

    /*
        Parse a single line of source assembly code.
    */

    enum LineTag
    {
        NONE = 0,
        DIR_TAG,
        INSTR_TAG,
        LABEL_ONLY_TAG,
        LABEL_DIR_TAG,
        LABEL_INSTR_TAG
    };

    typedef struct Line
    {
        token_container_t tokens;
        int lineNumber;
        LineTag tag;
    } Line_t;

    void removeCommentsFromLine(std::string &line);                                     /* removes characters after the '#' symbol */
    void trimLine(std::string &line);                                                   /* removes leading and trailing whitespace in the line */
    void tokenizeLine(std::string &line, int lineNumber, token_container_t &container); /* splits the current line into tokens */
    bool isLabelDeclaredAt(token_container_t &tokens, int tokenIndex);                  /* check if firs token in line is a label token */

    /* 
        Parse tokens and literals.
    */

    void tokenToLowerCase(token_t &token);                          /* converts characters in the token to lowercase */
    ASM::Instr isValidInstruction(token_t &token);                  /* checks if token holds a valid assembly instruction and returns its ID */
    bool isValidRegisterReference(token_t &token);                  /* checks if token holds a valid reference to CPU register */
    bool isIndirectRegisterAddrReference(token_t &token);           /* checks if token holds a registry indirect addressing reference */
    bool isValidSymbolName(token_t &token);                         /* checks if token holds a valid symbol name */
    ASM::word_t assertParseLiteral(token_t &token, int lineNumber); /* checks if token holds a valid literal and returns its decimal value */
    bool isDirectiveDeclaration(token_t &token);                    /* checks if token holds a section directive declaration */

    /* 
        Assembler semantic operations.
    */

    bool remainingTokenCountMatches(token_container_t &tokens, int targetCount, int countFrom);                   /* checks if the remaining token count matches the target count starting from the countFrom index */
    int setLabelOffsetFlag(token_container_t &tokens);                                                            /* asserts which offset should be used depending on the presence of label tokens */
    std::string encodeLiteralToBytecode(ASM::word_t literal);                                                     /* encodes given decimal literal into a hex string representation */
    void assertValidLabelDeclarationOn(int lineNumber, token_t &token, section_name_t &section);                  /* asserts that the token contains a valid label, otherwise throws exception */
    ASM::Instr assertValidInstruction(token_t &token, int lineNumber);                                            /* returns a valid ASM instruction identifier, otherwise throw sexception */
    void assertValidInstructionCall(ASM::Instr instr, section_name_t &section, int operandCount, int lineNumber); /* asserts if instruction was declared correctly, otherwise throws an exception */
    int getInstructionSize(ASM::Instr instr, token_container_t &tokens);                                          /* returns the correct byte size of the specified instruction */
    ASM::Directive assertValidDirective(token_t &token, int lineNumber);                                          /* returns the correct directive ID, otherwise throws an exception */
    void assertValidSectionDeclaration(token_container_t &tokens, int labelOffset, int lineNumber);               /* asserts that directive is declared correctly, otherwise throws an exception*/
    int calculateWordDirectiveOffset(token_container_t &tokens, int labelOffset, int lineNumber);                 /* returns the correct offset when the .word directive is used, otherwise throws an exception on invalid initializers */
    void assertSkipDirectiveOperandDeclared(token_container_t &tokens, int labelOffset, int lineNumber);          /* asserts that a operand for the .skip directive is declared, otherwise throws an exception */
    void assertEquDirectiveCorrectOperandCount(token_container_t &tokens, int labelOffset, int lineNumber);       /* asserts that the number of operands for the .equ directive is correct, otherwise throws an exception */

    /*
        Assembler data structures and internal assembler operations.
    */

    struct Section
    {
        static section_name_t NO_SECTION, ABSOLUTE;
    };
    section_name_t Section::NO_SECTION = "_NO_SECTION_";
    section_name_t Section::ABSOLUTE = "_ABSOLUTE_";

    typedef struct SymbolTableRow
    {
        static int ORDER_COUNTER;

        section_name_t section;
        int offset;
        bool isLocal;
        int orderID;

        void printRow()
        {
            std::cout << '{' << section << ", " << offset << ", " << (isLocal ? "local" : "global") << ", " << orderID << '}';
        }
    } SymbolTableRow_t;
    int Assembler::SymbolTableRow::ORDER_COUNTER = 0;

    typedef struct SectionTableRow
    {
        int offset;
        int size;
        std::string bytecode;
        std::string description;

        void printRow()
        {
            std::cout << '{' << offset << ", " << size << ", " << bytecode << ", " << description << '}';
        }
    } SectionTableRow_t;

    typedef struct SectionTable
    {
        int locationCounter;
        std::vector<SectionTableRow_t> row;
    } SectionTable_t;

    typedef std::pair<symbol_name_t, SymbolTableRow_t> symbol_table_row_pair_t;
    typedef std::pair<section_name_t, SectionTable_t> section_table_pair_t;
    typedef std::vector<Line_t> lines_t;

    std::map<symbol_name_t, SymbolTableRow_t> SymbolTable;
    std::map<section_name_t, SectionTable_t> SectionTables;

    void assertInputFileExists(std::ifstream &file);
    void initializeAssemblerTables();                                                         /* inserts default values inside the assembler tables */
    void createSectionTable(section_name_t &section);                                         /* adds new section table */
    void updateSectionLocationCounter(section_name_t &section, int addByteCount);             /* increases section location counter by addByteCount */
    void initBytesWithZero(section_name_t &section, int byteCount);                           /* allocates byteCount number of bytes and initializes them with 0 */
    void assertSymbolUndeclared(symbol_name_t &symbol, int lineNumber);                       /* will throw an error if the symbol has been already declared */
    void insertAbsoluteSymbol(symbol_name_t &symbol, ASM::word_t literal, int lineNumber);    /* inserts symbol with its initial value into the absolute section and returns operation success */
    void insertSection(section_name_t &section, int lineNumber);                              /* insert section name into symbol table */
    void insertLabel(symbol_name_t &label, section_name_t &section, int lineNumber);          /* insert label into symbol table */
    int prepareForNextLine(token_container_t &tokens, int lineCounter);                       /* clears the tokens and increments the line number counter */
    void storeLine(lines_t &program, token_container_t &tokens, int lineNumber, LineTag tag); /* stores line data into run memory */
    void patchLineTag(lines_t &program, LineTag tag);                                         /* patches the last pushed line with the correct tag */
    void resetSectionLocationCounters();                                                      /* resets the location counter in each section table */

    /* second pass processing */
    void processDirective(token_container_t &tokens, int labelOffset, int lineNumber, section_name_t &section);        /* wraps directive declaration processing */
    void processExternDirective(token_container_t &tokens, int labelOffset, int lineNumber);                           /* processes the .extern directive */
    void processGlobalDirective(token_container_t &tokens, int labelOffset, int lineNumber);                           /* processes the .global directive */
    void processSectionDirective(token_container_t &tokens, int labelOffset, int lineNumber, section_name_t &section); /* processes the .section directive */
    bool endDirectiveDetected(token_container_t &tokens, int labelOffset, int lineNumber);                             /* detects if an .end directive is present in the tokens */
    void assertProgramHasEnd(bool endDirectiveDetected, int lineNumber);                                               /* asserts that an .end directive has been declared in the program, otherwise throws an error */
    void processInstruction(token_container_t &tokens, int labelOffset, int lineNumber, section_name_t &section);      /* wraps instruction declaration processing */

}

namespace DebugAssembler
{
    void debugTokenization(Assembler::lines_t &program)
    {
        for (Assembler::Line_t &line : program)
        {
            for (std::string &token : line.tokens)
            {
                std::cout << "|" << token << "|, ";
            }

            switch (line.tag)
            {
            case Assembler::LineTag::INSTR_TAG:
                std::cout << "[INSTR]";
                break;
            case Assembler::LineTag::DIR_TAG:
                std::cout << "[DIR]";
                break;
            case Assembler::LineTag::LABEL_ONLY_TAG:
                std::cout << "[ONLY_LABEL]";
                break;
            case Assembler::LineTag::LABEL_DIR_TAG:
                std::cout << "[LABEL_DIR]";
                break;
            case Assembler::LineTag::LABEL_INSTR_TAG:
                std::cout << "[LABEL_INSTR]";
                break;
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }

    void debugSymbolTable()
    {
        std::cout << "Symbol table contents: " << std::endl;
        std::cout << "{symbol, {section, offset, isLocal, orderId}}" << std::endl
                  << std::endl;
        for (auto &row : Assembler::SymbolTable)
        {
            std::cout << '{' << row.first << ", ";
            row.second.printRow();
            std::cout << '}' << std::endl;
        }
        std::cout << std::endl;
    }

    void debugSectionTables()
    {
        std::cout << "Section table content: " << std::endl;
        std::cout << "{offset, size, bytecode, description}" << std::endl
                  << std::endl;
        for (auto &row : Assembler::SectionTables)
        {
            std::cout << "Section table: " << row.first << std::endl;
            for (auto &section : row.second.row)
            {
                section.printRow();
                std::cout << std::endl;
            }
            std::cout << std::endl;
        }
    }
}

#endif