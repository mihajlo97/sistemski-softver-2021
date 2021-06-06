#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
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

void Assembler::assertCorrectProgramCall(int argc, char *argv[])
{
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
    std::cerr << "Error @ line " << lineNumber << " with message: " << std::endl;
    std::cerr << '\t' << Assembler::WarningMessages[code] << std::endl;
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

void Assembler::tokenizeLine(std::string &line, int lineNumber, Assembler::token_container_t &tokenContainer)
{
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
        tokenContainer.pop_back();
    }
}

bool Assembler::isLabelDeclaredAt(Assembler::token_container_t &tokens, int tokenIndex)
{
    if (tokenIndex >= tokens.size())
    {
        return false;
    }

    Assembler::token_t t = tokens.at(tokenIndex);
    return t[t.length() - 1] == ASM::ReservedSymbols[ASM::Symbol::COLON];
}

/* parse tokens */
void Assembler::tokenToLowerCase(Assembler::token_t &token)
{
    std::transform(
        token.begin(),
        token.end(),
        token.begin(),
        [](unsigned char c)
        { return std::tolower(c); } /* */
    );
}

ASM::Instr Assembler::isValidInstruction(Assembler::token_t &token)
{
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

bool Assembler::isValidRegisterReference(Assembler::token_t &token)
{
    bool regFound = false;

    for (const auto &reg : ASM::Register)
    {
        if (token == reg)
        {
            regFound = true;
            break;
        }
    }

    return regFound;
}

bool Assembler::isIndirectRegisterAddrReference(Assembler::token_t &token)
{
    int bracketStart, bracketEnd;

    bracketStart = token.find_first_of(ASM::ReservedSymbols[ASM::Symbol::BRACKET_OPEN]);
    bracketEnd = token.find_last_of(ASM::ReservedSymbols[ASM::Symbol::BRACKET_CLOSE]);

    if (bracketStart == std::string::npos || bracketEnd == std::string::npos)
    {
        return false;
    }

    Assembler::token_t reg = token.substr(bracketStart + 1, bracketEnd - bracketStart - 1);
    return isValidRegisterReference(reg);
}

ASM::Directive Assembler::assertValidDirective(Assembler::token_t &token, int lineNumber)
{
    bool directiveFound = false;
    int directiveID = 0;

    Assembler::tokenToLowerCase(token);
    for (const auto &directive : ASM::Directives)
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

    if (directive == ASM::Directive::INVALID_DIRECTIVE)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNKNOWN_DIRECTIVE);
    }

    return directive;
}

bool Assembler::isValidSymbolName(Assembler::token_t &token)
{
    const std::regex identTemplate("[a-zA-Z_][a-zA-Z0-9_]*");

    return std::regex_match(token, identTemplate);
}

ASM::word_t Assembler::assertParseLiteral(Assembler::token_t &token, int lineNumber)
{
    // return the bit representation of the literal inside the token or throw exception on any non-valid literal

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
        if (value >= 2 << ASM::MAX_DATA_WIDTH || value < (-(2 << ASM::MAX_DATA_WIDTH)))
        {
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }
    }
    else if (std::regex_match(token, binary))
    {
        if (token.length() - 2 > ASM::MAX_DATA_WIDTH)
        {
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t j = 0;
        for (int i = token.length() - 1; token[i] != 'b' && token[i] != 'B'; i--, j++)
        {
            if (j >= ASM::MAX_DATA_WIDTH)
            {
                break;
            }

            value |= ((token[i] - '0') << j);
        }
    }
    else if (std::regex_match(token, oct))
    {
        if (token.length() - 1 >= ASM::MAX_DATA_WIDTH / 3)
        {
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t j = 0;
        for (int i = token.length() - 1; i > 0; i--, j++)
        {
            if (j > ASM::MAX_DATA_WIDTH / 3)
            {
                break;
            }

            value |= (token[i] - '0') << (3 * j);
        }
    }
    else if (std::regex_match(token, hex))
    {
        if (token.length() - 2 > ASM::MAX_DATA_WIDTH / 4)
        {
            Assembler::reportWarning(lineNumber, Assembler::WarningCode::TRUNCATE);
        }

        value = 0;
        ASM::word_t aux, j = 0;
        for (int i = token.length() - 1; i > 1; i--, j++)
        {
            if (j >= ASM::MAX_DATA_WIDTH / 4)
            {
                break;
            }

            if (token[i] - '0' < 10)
            {
                value |= (token[i] - '0') << (4 * j);
            }
            else if (token[i] - 'a' < 6)
            {
                value |= ((token[i] - 'a') + 10) << (4 * j);
            }
            else
            {
                value |= ((token[i] - 'A') + 10) << (4 * j);
            }
        }
    }
    else
    {
        throw Assembler::ErrorCode::INVALID_LITERAL;
    }

    return value;
}

bool Assembler::isSectionDeclaration(Assembler::token_t &token)
{
    return token[0] == ASM::ReservedSymbols[ASM::Symbol::DOT];
}

/* assembler semantic operations */
void Assembler::assertValidLabelDeclarationOn(int lineNumber, Assembler::token_t &token, Assembler::section_name_t &section)
{
    if (!Assembler::isValidSymbolName(token))
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
    }
    else if (section == Assembler::Section::NO_SECTION)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::NO_SECTION);
    }
}

ASM::Instr Assembler::assertValidInstruction(token_t &token, int lineNumber)
{
    ASM::Instr instrID = Assembler::isValidInstruction(token);

    if (instrID == ASM::Instr::INVALID_INSTR)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNKNOWN_INSTRUCTION);
    }

    return instrID;
}

void Assembler::assertValidInstructionCall(ASM::Instr instr, Assembler::section_name_t &section, int operandCount, int lineNumber)
{
    if (section == Assembler::Section::NO_SECTION)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::NO_SECTION);
    }

    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];

    if (operandCount != instrDesc.operandCount)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNSUPPORTED_OPERANDS);
    }
}

int Assembler::getInstructionSize(ASM::Instr instr, Assembler::token_container_t &tokens)
{
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    Assembler::token_t lastOperand;
    int instrSize;

    if (!instrDesc.allowOtherAddrModes)
    {
        instrSize = instrDesc.size;
    }
    else
    {
        lastOperand = tokens[tokens.size() - 1];
        instrSize = (Assembler::isValidRegisterReference(lastOperand) ||
                     Assembler::isIndirectRegisterAddrReference(lastOperand))
                        ? instrDesc.size
                        : instrDesc.altSize;
    }

    return instrSize;
}

bool Assembler::remainingTokenCountMatches(Assembler::token_container_t &tokens, int targetCount, int countFrom)
{
    return tokens.size() - countFrom == targetCount;
}

int Assembler::setLabelOffsetFlag(Assembler::token_container_t &tokens)
{
    return Assembler::isLabelDeclaredAt(tokens, 0);
}

std::string Assembler::encodeLiteralToBytecode(ASM::word_t literal)
{
    // formatting is little endian

    std::string result, bytecode;
    ASM::word_t aux, mask = 0x000F;
    char hexChar;

    for (int i = ASM::MAX_DATA_WIDTH / 4 - 1; i >= 0; i--)
    {
        aux = (literal >> (4 * i)) & mask;

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

        bytecode.push_back(hexChar);
    }

    result = bytecode.substr(2, 2);
    result.push_back(' ');
    result += bytecode.substr(0, 2);

    return result;
}

void Assembler::assertValidSectionDeclaration(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    if (tokens.size() > 1 + labelOffset)
    {
        if (!Assembler::isValidSymbolName(tokens[labelOffset + 1]))
        {
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
        }
    }
    else
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
    }
}

int Assembler::calculateWordDirectiveOffset(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    int offset = 0;

    for (int i = labelOffset + 1; i < tokens.size(); i++)
    {
        if (Assembler::isValidSymbolName(tokens.at(i)))
        {
            offset += ASM::InstrSize::WORD;
        }
        else
        {
            try
            {
                Assembler::assertParseLiteral(tokens.at(i), lineNumber);
                offset += ASM::InstrSize::WORD;
            }
            catch (Assembler::ErrorCode code)
            {
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_INITIALIZER);
            }
        }
    }

    return offset;
}

void Assembler::assertSkipDirectiveOperandDeclared(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    if (tokens.size() - labelOffset < 2)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_LITERAL);
    }
}

void Assembler::assertEquDirectiveCorrectOperandCount(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    if (tokens.size() > 2 + labelOffset)
    {
        if (!Assembler::isValidSymbolName(tokens[labelOffset + 1]))
        {
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
        }
    }
    else if (tokens.size() - labelOffset == 2)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_LITERAL);
    }
    else if (tokens.size() - labelOffset == 1)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
    }
}

/* handle internal assembler structures and operations */
void Assembler::assertInputFileExists(std::ifstream &file)
{
    if (!file.is_open())
    {
        Assembler::reportErrorAndExit(Assembler::ErrorCode::FILE_NOT_FOUND);
    }
}

void Assembler::initializeAssemblerTables()
{
    // an absolute section is generated by default for local named literals
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
    auto &sectionTable = *(Assembler::SectionTables.find(section));
    sectionTable.second.locationCounter += addByteCount;
}

void Assembler::initBytesWithZero(section_name_t &section, int byteCount)
{
    auto &sectionTable = *(Assembler::SectionTables.find(section));
    std::string bytecode = "";

    for (int i = 0; i < byteCount; i++)
    {
        bytecode += "00";
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
    if (Assembler::SymbolTable.count(symbol) > 0)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::DUPLICATE_IDENTIFIER);
    }
}

void Assembler::insertAbsoluteSymbol(symbol_name_t &symbol, ASM::word_t literal, int lineNumber)
{
    Assembler::assertSymbolUndeclared(symbol, lineNumber);

    auto &sectionTable = *(Assembler::SectionTables.find(Assembler::Section::ABSOLUTE));
    int locCounter = sectionTable.second.locationCounter;

    Assembler::symbol_table_row_pair_t row = {
        symbol,
        {Assembler::Section::ABSOLUTE,
         locCounter,
         true,
         Assembler::SymbolTableRow::ORDER_COUNTER++} /* */
    };
    Assembler::SymbolTable.insert(row);

    std::string desc;
    std::stringstream converter;
    converter << "Local named literal " << symbol << " = " << literal;
    desc = converter.str();

    sectionTable.second.row.push_back(
        {sectionTable.second.locationCounter,
         ASM::InstrSize::WORD,
         Assembler::encodeLiteralToBytecode(literal),
         desc} /* */
    );
    Assembler::updateSectionLocationCounter(Assembler::Section::ABSOLUTE, ASM::InstrSize::WORD);
}

void Assembler::insertSection(Assembler::section_name_t &section, int lineNumber)
{
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
    Assembler::assertSymbolUndeclared(label, lineNumber);

    auto sectionTable = *(Assembler::SectionTables.find(section));
    Assembler::symbol_table_row_pair_t symbolTableRow = {
        label,
        {section, sectionTable.second.locationCounter, true, Assembler::SymbolTableRow::ORDER_COUNTER++} /* */
    };
    Assembler::SymbolTable.insert(symbolTableRow);
}

int Assembler::prepareForNextLine(Assembler::token_container_t &tokens, int lineCounter)
{
    tokens.clear();
    return ++lineCounter;
}

void Assembler::storeTokens(lines_t &program, token_container_t &tokens, int lineNumber)
{
    program.push_back({tokens, lineNumber});
}

int main(int argc, char *argv[])
{
    Assembler::assertCorrectProgramCall(argc, argv);

    /* refrences to resource files */
    std::string inputFilePath((argc == 2) ? argv[1] : argv[3]);
    std::string outputFilePath((argc == 4) ? argv[2] : "");
    std::ifstream sourceProgram(inputFilePath);

    /* parsing lines from source file */
    int lineCounter = 1, labelOffset = 0;
    std::string programLine;
    Assembler::token_t token;
    Assembler::token_container_t tokens;
    Assembler::lines_t Program;

    /* parsing assembly semantics */
    int addByteCount, operandCount;
    ASM::word_t literalBits;
    Assembler::section_name_t currentSection = Assembler::Section::NO_SECTION;
    Assembler::symbol_table_row_pair_t symbolTableRow;

    Assembler::assertInputFileExists(sourceProgram);
    Assembler::initializeAssemblerTables();

    /* perform assembler first pass */
    try
    {
        while (std::getline(sourceProgram, programLine))
        {
            Assembler::removeCommentsFromLine(programLine);
            Assembler::trimLine(programLine);

            if (!programLine.empty())
            {
                Assembler::tokenizeLine(programLine, lineCounter, tokens);
            }
            else
            {
                lineCounter = Assembler::prepareForNextLine(tokens, lineCounter);
                continue;
            }

            Assembler::storeTokens(Program, tokens, lineCounter);
            token = tokens[0];

            if (Assembler::isLabelDeclaredAt(tokens, 0))
            {
                token.erase(token.length() - 1, 1);

                Assembler::assertValidLabelDeclarationOn(lineCounter, token, currentSection);
                Assembler::insertLabel(token, currentSection, lineCounter);

                // jump to next line when the line only holds a label
                if (tokens.size() == 1)
                {
                    lineCounter = Assembler::prepareForNextLine(tokens, lineCounter);
                    continue;
                }
            }

            // reference tokens corretly based on whether a label is at the start of the line
            labelOffset = Assembler::setLabelOffsetFlag(tokens);
            token = tokens[labelOffset];

            // parse remainder of the line
            if (Assembler::isSectionDeclaration(token))
            {
                ASM::Directive dir = Assembler::assertValidDirective(token, lineCounter);

                if (dir == ASM::Directive::DOT_END)
                {
                    // skip to the second pass of assembling
                    break;
                }

                switch (dir)
                {
                case ASM::Directive::DOT_GLOBAL:
                case ASM::Directive::DOT_EXTERN:
                    // skip directives during first pass
                    break;

                case ASM::Directive::DOT_SECTION:
                    Assembler::assertValidSectionDeclaration(tokens, labelOffset, lineCounter);
                    currentSection = tokens[labelOffset + 1];
                    Assembler::insertSection(currentSection, lineCounter);
                    break;

                case ASM::Directive::DOT_WORD:
                    // only calculate the offset, the initialization in the section table is done in the second pass
                    addByteCount = Assembler::calculateWordDirectiveOffset(tokens, labelOffset, lineCounter);
                    Assembler::updateSectionLocationCounter(currentSection, addByteCount);
                    break;

                case ASM::Directive::DOT_SKIP:
                    Assembler::assertSkipDirectiveOperandDeclared(tokens, labelOffset, lineCounter);
                    literalBits = Assembler::assertParseLiteral(tokens[labelOffset + 1], lineCounter);
                    Assembler::initBytesWithZero(currentSection, literalBits);
                    break;

                case ASM::Directive::DOT_EQU:
                {
                    Assembler::assertEquDirectiveCorrectOperandCount(tokens, labelOffset, lineCounter);
                    literalBits = Assembler::assertParseLiteral(tokens[labelOffset + 2], lineCounter);
                    Assembler::insertAbsoluteSymbol(tokens[labelOffset + 1], literalBits, lineCounter);
                    break;
                }
                }
            }
            else if (Assembler::isLabelDeclaredAt(tokens, 1))
            {
                Assembler::reportErrorAtLineAndThrow(lineCounter, Assembler::ErrorCode::MULTIPLE_LABELS);
            }
            else
            {
                ASM::Instr instrID = Assembler::assertValidInstruction(token, lineCounter);

                operandCount = tokens.size() - 1 - labelOffset;
                Assembler::assertValidInstructionCall(instrID, currentSection, operandCount, lineCounter);

                addByteCount = Assembler::getInstructionSize(instrID, tokens);
                Assembler::updateSectionLocationCounter(currentSection, addByteCount);
            }

            lineCounter = Assembler::prepareForNextLine(tokens, lineCounter);
        }
    }
    catch (Assembler::ErrorCode code)
    {
        sourceProgram.close();
        Assembler::reportErrorAndExit(code);
    }

    /* DEBUG */
    DebugAssembler::debugTokenization(Program);
    DebugAssembler::debugSymbolTable();
    DebugAssembler::debugSectionTables();

    return 0;
}