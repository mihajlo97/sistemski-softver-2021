#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>
#include <map>
#include <regex>
#include "../inc/assembler.hpp"

/* warning & error handling */
void Assembler::reportErrorAndExit(Assembler::ErrorCode code)
{
    std::cerr << Assembler::ErrorMessages[code] << std::endl;
    std::cerr << "Assembling failed, exiting program..." << std::endl
              << std::endl;
    exit(code + 1);
}

void Assembler::checkProgramArgs(int argc, char *argv[])
{
    // check if program is called correctly
    // usage: asembler [-o <output_file_name>] <input_file_name.s>
    if (argc != 2 && argc != 4)
    {
        Assembler::reportErrorAndExit(Assembler::ErrorCode::BAD_ARGS);
    }

    if (argc == 4)
    {
        std::string option("-o"), optionUpper("-O");

        if (option.compare(argv[1]) != 0 && optionUpper.compare(argv[1]) != 0)
        {
            Assembler::reportErrorAndExit(Assembler::ErrorCode::BAD_ARGS);
        }
    }

    std::string extension(".s"), extensionUpper(".S"), filename((argc == 2) ? argv[1] : argv[3]);

    if (filename.rfind(extension) == std::string::npos && filename.rfind(extensionUpper) == std::string::npos)
    {
        Assembler::reportErrorAndExit(Assembler::ErrorCode::BAD_ARGS);
    }
}

void Assembler::reportWarning(int lineNumber, Assembler::WarningCode code)
{
    std::cerr << Assembler::WarningMessages[code] << std::endl;
}

void Assembler::reportErrorAtLineAndThrow(int lineNumber, Assembler::ErrorCode code)
{
    std::cerr << "Error detected @ line " << lineNumber << " with the following message:" << std::endl
              << '\t';
    throw code;
}

/* parse source code line */
void Assembler::removeCommentsFromLine(std::string &line)
{
    int commentStartIndex;

    commentStartIndex = line.find_first_of(ASM::ReservedSymbols[ASM::Symbol::HASHTAG]);
    if (commentStartIndex != std::string::npos)
    {
        line = line.erase(commentStartIndex);
    }
}

void Assembler::trimLine(std::string &line)
{
    const char *const whitespace = " \t\n\r\f\v";

    line = line.erase(0, line.find_first_not_of(whitespace));
    line = line.erase(line.find_last_not_of(whitespace) + 1);
}

void Assembler::tokenizeLine(std::string &line, int lineNumber, std::vector<std::string> &tokenContainer)
{
    // break line into usable tokens for source code parsing
    const char *const delimeters = " ,\t\n\r\f\v";
    size_t currIndex, nextIndex = -1;

    do
    {
        // skip empty tokens
        nextIndex = line.find_first_not_of(delimeters, nextIndex + 1);
        if (nextIndex == std::string::npos)
        {
            break;
        }
        nextIndex -= 1;

        // copy substring as a token and store into vector container
        currIndex = nextIndex + 1;
        nextIndex = line.find_first_of(delimeters, currIndex);
        tokenContainer.push_back(line.substr(currIndex, nextIndex - currIndex));

    } while (nextIndex != std::string::npos);

    // merge tokens when using [reg + offset] addressing mode
    bool offsetAddrMode = false;
    int plusSignIndex;
    for (int i = 1; i < tokenContainer.size(); i++)
    {
        // detect a lone '+' token and start merging tokens to the token before the '+' token
        if (tokenContainer.at(i)[0] == ASM::ReservedSymbols[ASM::Symbol::PLUS] && !offsetAddrMode)
        {
            offsetAddrMode = true;
            plusSignIndex = i;
            tokenContainer.at(plusSignIndex - 1).append(tokenContainer.at(i));
            continue;
        }
        if (offsetAddrMode)
        {
            tokenContainer.at(plusSignIndex - 1).append(tokenContainer.at(i));
        }
    }
    for (int j = tokenContainer.size() - 1; j >= plusSignIndex && offsetAddrMode; j--)
    {
        // remove merged tokens
        tokenContainer.pop_back();
    }
}

/* parse tokens */
void Assembler::tokenToLowerCase(std::string &token)
{
    std::transform(
        token.begin(),
        token.end(),
        token.begin(),
        [](unsigned char c)
        { return std::tolower(c); } /* */
    );
}

ASM::Instr Assembler::isValidInstruction(std::string &token)
{
    // if valid instruction, return its ID, otherwise return an INVALID_INSTR ID
    bool instrFound = false;
    int instrID = 0;

    Assembler::tokenToLowerCase(token);
    for (const std::string &instr : ASM::Instruction)
    {
        if (token == instr)
        {
            instrFound = true;
            break;
        }
        instrID++;
    }

    ASM::Instr instruction = static_cast<ASM::Instr>((instrFound) ? instrID : ASM::Instr::INVALID_INSTR);

    return instruction;
}

bool Assembler::isValidRegisterReference(std::string &token)
{
    bool regFound = false;

    for (const std::string &reg : ASM::Register)
    {
        if (token == reg)
        {
            regFound = true;
            break;
        }
    }

    return regFound;
}

bool Assembler::isIndirectRegisterAddrReference(std::string &token)
{
    // check if token holds an indirect registry addressing mode operand
    int bracketStart, bracketEnd;

    bracketStart = token.find_first_of(ASM::ReservedSymbols[ASM::Symbol::BRACKET_OPEN]);
    bracketEnd = token.find_last_of(ASM::ReservedSymbols[ASM::Symbol::BRACKET_CLOSE]);

    if (bracketStart == std::string::npos || bracketEnd == std::string::npos)
    {
        return false;
    }

    std::string reg = token.substr(bracketStart + 1, bracketEnd - bracketStart - 1);
    return isValidRegisterReference(reg);
}

ASM::Directive Assembler::isValidDirective(std::string &token)
{
    // fetch appropriate directive ID or return invalid directive ID
    bool directiveFound = false;
    int directiveID = 0;

    Assembler::tokenToLowerCase(token);
    for (const std::string &directive : ASM::Directives)
    {
        if (token == directive)
        {
            directiveFound = true;
            break;
        }
        directiveID++;
    }

    ASM::Directive directive = static_cast<ASM::Directive>((directiveFound)
                                                               ? directiveID
                                                               : ASM::Directive::INVALID_DIRECTIVE);
    return directive;
}

bool Assembler::isValidSymbolName(std::string &token)
{
    const std::regex identTemplate("[a-zA-Z_][a-zA-Z0-9_]*");

    return std::regex_match(token, identTemplate);
}

ASM::word_t Assembler::assertParseLiteral(std::string &token, int lineNumber)
{
    // check if token holds a valid 16 bit literal in various formats
    // the function will return the 16 bit representation of the literal or an invalid literal value
    std::regex number("(-)*[1-9][0-9]*"), binary("0[bB][0-1]+"), oct("0[0-7]+"),
        hex("0[xX][0-9a-fA-F]+"), character("\'[\x21-\x7E]\'");
    ASM::word_t value;

    if (std::regex_match(token, character))
    {
        value = token[1];
    }
    else if (std::regex_match(token, number))
    {
        value = std::stoi(token);
        if (value >= 2 << ASM::MAX_DATA_WIDTH || value < (~(2 << ASM::MAX_DATA_WIDTH) + 1))
        {
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }
    }
    else if (std::regex_match(token, binary))
    {
        if (token.length() - 2 > ASM::MAX_DATA_WIDTH)
        {
            // more than 16 bits after 0b / 0B results in the value being truncated
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t j = 0;
        for (int i = token.length() - 1; token[i] != 'b' || token[i] != 'B'; i--)
        {
            value += (token[i] - '0') * (1 << j++);
            if (j >= ASM::MAX_DATA_WIDTH)
            {
                break;
            }
        }
    }
    else if (std::regex_match(token, oct))
    {
        if (token.length() - 1 >= ASM::MAX_DATA_WIDTH / 3)
        {
            // more than 5 octal numbers after 0 results in the value being truncated
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t aux, j = 0;
        for (int i = token.length() - 1; i > 0; i--, j++)
        {
            if (j > ASM::MAX_DATA_WIDTH / 3)
            {
                break;
            }

            for (int k = 0, aux = 1; k < j; k++)
            {
                aux *= 8;
            }
            value += (token[i] - '0') * aux;
        }
    }
    else if (std::regex_match(token, hex))
    {
        if (token.length() - 2 >= ASM::MAX_DATA_WIDTH / 4)
        {
            // more than 4 hex numbers after 0x / 0X results in the value being truncated
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t aux, j = 0;
        for (int i = token.length() - 1; i > 1; i--, j++)
        {
            if (j > ASM::MAX_DATA_WIDTH / 4)
            {
                break;
            }

            for (int k = 0, aux = 1; k < j; k++)
            {
                aux *= 16;
            }

            if (token[i] - '0' < 10)
            {
                value += (token[i] - '0') * aux;
            }
            else if (token[i] - 'a' < 6)
            {
                value += (token[i] - 'a' + 10) * aux;
            }
            else
            {
                value += (token[i] - 'A' + 10) * aux;
            }
        }
    }
    else
    {
        throw Assembler::ErrorCode::INVALID_LITERAL;
    }

    return value;
}

std::string Assembler::encodeDecToHex(ASM::word_t literal)
{
    // encodes literal into a string of hex code values
    std::string result;
    ASM::word_t aux, mask = 0x000F;
    char hexChar;

    for (int i = 0; i < ASM::MAX_DATA_WIDTH / 4; i++)
    {
        aux = literal & (mask << (4 * i));
        aux >>= (4 * i);

        switch (aux)
        {
        case 0x0000:
            hexChar = '0';
            break;
        case 0x0001:
            hexChar = '1';
            break;
        case 0x0002:
            hexChar = '2';
            break;
        case 0x0003:
            hexChar = '3';
            break;
        case 0x0004:
            hexChar = '4';
            break;
        case 0x0005:
            hexChar = '5';
            break;
        case 0x0006:
            hexChar = '6';
            break;
        case 0x0007:
            hexChar = '7';
            break;
        case 0x0008:
            hexChar = '8';
            break;
        case 0x0009:
            hexChar = '9';
            break;
        case 0x000A:
            hexChar = 'A';
            break;
        case 0x000B:
            hexChar = 'B';
            break;
        case 0x000C:
            hexChar = 'C';
            break;
        case 0x000D:
            hexChar = 'D';
            break;
        case 0x000E:
            hexChar = 'E';
            break;
        case 0x000F:
            hexChar = 'F';
            break;
        default:
            hexChar = '?';
            break;
        }

        result.push_back(hexChar);
    }

    return result;
}

/* handle internal assembler structures and operations */
void Assembler::initializeAssemblerTables()
{
    // add absolute section for local literals
    section_table_pair_t absoluteSection = {
        Assembler::Section::ABSOLUTE,
        {0, std::vector<Assembler::SectionTableRow_t>()} /* */
    };
    Assembler::SectionTables.insert(absoluteSection);
}

void Assembler::createSectionTable(section_name_t &section)
{
    section_table_pair_t sectionTable = {
        section,
        {0, std::vector<Assembler::SectionTableRow_t>()} /* */
    };
    Assembler::SectionTables.insert(sectionTable);
}

void Assembler::updateSectionLocationCounter(section_name_t &section, int addByteCount)
{
    auto sectionTable = *(Assembler::SectionTables.find(section));
    sectionTable.second.locationCounter += addByteCount;
}

void Assembler::initBytesWithZero(section_name_t &section, int byteCount)
{
    // adds byteCount number of '00' bytes to specified section

    auto sectionTable = *(Assembler::SectionTables.find(section));
    std::string bytecode;

    for (int i = 0; i < byteCount; i++)
    {
        bytecode.push_back('0');
        bytecode.push_back('0');
        if (i + 1 < byteCount)
        {
            bytecode.push_back(' ');
        }
    }

    sectionTable.second.row.push_back(
        {sectionTable.second.locationCounter,
         byteCount,
         bytecode,
         "Unused bytes initialized to 0 with .skip directive."} /* */
    );
    sectionTable.second.locationCounter += byteCount;
}

void Assembler::assertSymbolUndeclared(Assembler::symbol_name_t &symbol, int lineNumber)
{
    // stop assembling if symbol is already declared
    if (Assembler::SymbolTable.count(symbol) == 1)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::DUPLICATE_IDENTIFIER);
    }
}

void Assembler::insertAbsoluteSymbol(symbol_name_t &symbol, ASM::word_t literal, int lineNumber)
{
    // function adds declared local symbol to symbol table and absolute section table

    Assembler::assertSymbolUndeclared(symbol, lineNumber);

    auto sectionTable = *(Assembler::SectionTables.find(Assembler::Section::ABSOLUTE));
    int locCounter = sectionTable.second.locationCounter;
    std::string bytecode, desc, aux = Assembler::encodeDecToHex(literal);

    // insert new symbol into symbol table
    Assembler::symbol_table_row_pair_t row = {
        symbol,
        {Assembler::Section::ABSOLUTE,
         locCounter,
         true,
         Assembler::SymbolTableRow::ORDER_COUNTER++} /* */
    };
    Assembler::SymbolTable.insert(row);

    // fetch single bytes and order them in little endian format
    bytecode = aux.substr(2, 2);
    bytecode.push_back(' ');
    bytecode += aux.substr(0, 2);

    desc = "Absolute symbol " + symbol + " = ";
    desc += literal;
    desc += " ;";

    // add symbol to absolute section
    sectionTable.second.row.push_back(
        {sectionTable.second.locationCounter,
         ASM::InstrSize::WORD,
         bytecode,
         desc} /* */
    );
    sectionTable.second.locationCounter += ASM::InstrSize::WORD;
}

void Assembler::insertSection(Assembler::section_name_t &section, int lineNumber)
{
    // function adds declared section into symbol table if not previously declared
    // a section content table is created for the specified section

    Assembler::assertSymbolUndeclared(section, lineNumber);

    Assembler::symbol_table_row_pair_t symbolTableRow = {
        section,
        {section, 0, true, Assembler::SymbolTableRow::ORDER_COUNTER++} /* */
    };
    Assembler::SymbolTable.insert(symbolTableRow);

    Assembler::createSectionTable(section);
}

void Assembler::insertLabel(Assembler::symbol_name_t &label, section_name_t &section, int lineNumber)
{
    // function adds declared label into the symbol table

    Assembler::assertSymbolUndeclared(label, lineNumber);

    auto sectionTable = *(Assembler::SectionTables.find(section));
    Assembler::symbol_table_row_pair_t symbolTableRow = {
        label,
        {section, sectionTable.second.locationCounter, true, Assembler::SymbolTableRow::ORDER_COUNTER++} /* */
    };
    Assembler::SymbolTable.insert(symbolTableRow);
}

int main(int argc, char *argv[])
{
    Assembler::checkProgramArgs(argc, argv);

    /* refrences to input/output files */
    std::string inputFilePath((argc == 2) ? argv[1] : argv[3]);
    std::string outputFilePath((argc == 4) ? argv[2] : "");
    std::ifstream sourceProgram(inputFilePath);

    /* parsing lines from source file */
    int lineCounter = 0, asmConstructStart = 0;
    std::string programLine;
    std::string token;
    char tokenStart, tokenEnd;
    std::vector<std::string> tokens;
    Assembler::Line_t Line;
    std::vector<Assembler::Line_t> Program;

    /* parsing assembly semantics */
    int addByteCount;
    ASM::word_t literalDecimalValue;
    ASM::InstrDescTable_t instrDesc;
    std::string operand;
    std::string currentSection = Assembler::Section::NO_SECTION;
    Assembler::symbol_table_row_pair_t symbolTableRow;

    if (!sourceProgram.is_open())
    {
        Assembler::reportErrorAndExit(Assembler::ErrorCode::FILE_NOT_FOUND);
    }

    Assembler::initializeAssemblerTables();

    /* perform assembler first pass */
    try
    {
        while (std::getline(sourceProgram, programLine))
        {
            Assembler::removeCommentsFromLine(programLine);
            Assembler::trimLine(programLine);

            if (programLine.empty())
            {
                lineCounter++;
                tokens.clear();
                continue;
            }
            else
            {
                Assembler::tokenizeLine(programLine, lineCounter, tokens);
            }

            // store tokens for the second pass of assembling
            Line.lineNumber = lineCounter;
            Line.tokens = tokens;
            Program.push_back(Line);

            token = tokens[0];
            tokenStart = token[0];
            tokenEnd = token[token.length() - 1];

            // parse when line starts with label
            if (tokenEnd == ASM::ReservedSymbols[ASM::Symbol::COLON])
            {
                token.erase(token.length() - 1, 1);

                if (!Assembler::isValidSymbolName(token))
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_IDENTIFIER);
                }
                else if (currentSection == Assembler::Section::NO_SECTION)
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::NO_SECTION);
                }

                Assembler::insertLabel(token, currentSection, lineCounter);

                // jump to next line if only a label has been declared in this line
                if (tokens.size() == 1)
                {
                    lineCounter++;
                    tokens.clear();
                    continue;
                }
                else
                {
                    // reference tokens corretly based on whether a label is at the start of the line
                    asmConstructStart = 1;
                    token = tokens[asmConstructStart];
                    tokenStart = token[0];
                    tokenEnd = token[token.length() - 1];
                }
            }
            else
            {
                asmConstructStart = 0;
            }

            // parse remainder of the line
            if (tokenStart == ASM::ReservedSymbols[ASM::Symbol::DOT])
            {
                // parse token as directive
                ASM::Directive dir = Assembler::isValidDirective(token);

                if (dir == ASM::Directive::DOT_END)
                {
                    // skip to the second pass of assembling
                    break;
                }

                // parse declared directive
                switch (dir)
                {
                case ASM::Directive::DOT_GLOBAL:
                case ASM::Directive::DOT_EXTERN:
                {
                    // skip directives during first pass
                    break;
                }

                case ASM::Directive::DOT_SECTION:
                {
                    if (tokens.size() > 1 + asmConstructStart)
                    {
                        if (!Assembler::isValidSymbolName(tokens[asmConstructStart + 1]))
                        {
                            Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_IDENTIFIER);
                        }
                    }
                    else
                    {
                        Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_IDENTIFIER);
                    }

                    currentSection = tokens[asmConstructStart + 1];
                    Assembler::insertSection(currentSection, lineCounter);

                    break;
                }

                case ASM::Directive::DOT_WORD:
                {
                    // increase location counter of current section by the number of symbols/literals declared
                    // leave initialization for the second pass of assembling
                    addByteCount = 0;

                    for (int i = asmConstructStart + 1; i < tokens.size(); i++)
                    {
                        if (Assembler::isValidSymbolName(tokens.at(i)) ||
                            Assembler::assertParseLiteral(tokens.at(i), lineCounter))
                        {
                            addByteCount += ASM::InstrSize::WORD;
                        }
                        else
                        {
                            Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_INITIALIZER);
                        }
                    }

                    Assembler::updateSectionLocationCounter(currentSection, addByteCount);

                    break;
                }

                case ASM::Directive::DOT_SKIP:
                {
                    if (tokens.size() - asmConstructStart < 2)
                    {
                        Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_LITERAL);
                    }
                    literalDecimalValue = Assembler::assertParseLiteral(tokens[asmConstructStart + 1], lineCounter);

                    Assembler::initBytesWithZero(currentSection, literalDecimalValue);

                    break;
                }

                case ASM::Directive::DOT_EQU:
                {
                    if (tokens.size() > 2 + asmConstructStart)
                    {
                        if (!Assembler::isValidSymbolName(tokens[asmConstructStart + 1]))
                        {
                            Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_IDENTIFIER);
                        }

                        literalDecimalValue = Assembler::assertParseLiteral(tokens[asmConstructStart + 2], lineCounter);
                    }
                    else if (tokens.size() - asmConstructStart == 2)
                    {
                        Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_LITERAL);
                    }
                    else if (tokens.size() - asmConstructStart == 1)
                    {
                        Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::INVALID_IDENTIFIER);
                    }

                    Assembler::insertAbsoluteSymbol(tokens[asmConstructStart + 1], literalDecimalValue, lineCounter);

                    break;
                }

                default:
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::UNKNOWN_DIRECTIVE);
                    break;
                }
                }
            }
            else if (tokenEnd == ASM::ReservedSymbols[ASM::Symbol::COLON])
            {
                Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::MULTIPLE_LABELS);
            }
            else
            {
                // parse token as instruction
                ASM::Instr instrID = Assembler::isValidInstruction(token);

                if (instrID == ASM::Instr::INVALID_INSTR)
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::UNKNOWN_INSTRUCTION);
                }

                if (currentSection == Assembler::Section::NO_SECTION)
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::NO_SECTION);
                }

                instrDesc = ASM::InstructionDescTable[instrID];

                if (tokens.size() - 1 - asmConstructStart != instrDesc.operandCount)
                {
                    Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::UNSUPPORTED_OPERANDS);
                }

                // check which addressing mode is utilized to fetch correct instruction size
                if (!instrDesc.allowOtherAddrModes)
                {
                    addByteCount = instrDesc.size;
                }
                else
                {
                    operand = tokens[tokens.size() - 1];
                    addByteCount = (Assembler::isValidRegisterReference(operand) ||
                                    Assembler::isIndirectRegisterAddrReference(operand))
                                       ? instrDesc.size
                                       : instrDesc.altSize;
                }

                Assembler::updateSectionLocationCounter(currentSection, addByteCount);
            }

            // DEBUG
            if (!programLine.empty())
            {
                std::cout << programLine << std::endl;
                /*for (std::string &token : Line.tokens)
                {
                    std::cout << "|" << token << "|" << std::endl;
                }
                std::cout << std::endl;*/
            }

            // reset for next iteration
            tokens.clear();
            lineCounter++;
        }
    }
    catch (Assembler::ErrorCode code)
    {
        sourceProgram.close();
        Assembler::reportErrorAndExit(code);
    }

    for (Assembler::Line_t &line : Program)
    {
        for (std::string &token : line.tokens)
        {
            std::cout << "|" << token << "|, ";
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;

    /* DEBUG */
    int exit;
    std::cin >> exit;

    return 0;
}