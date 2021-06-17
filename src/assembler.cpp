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
    std::cerr << "Warning @ line " << lineNumber << " with message: " << std::endl;
    std::cerr << '\t' << Assembler::WarningMessages[code] << std::endl;
}

void Assembler::reportErrorAtLineAndThrow(int lineNumber, Assembler::ErrorCode code)
{
    std::cerr << "Error detected @ line " << lineNumber << " with the following message:" << std::endl
              << '\t';
    throw code;
}

void Assembler::willThrowOnToken(Assembler::token_t &token)
{
    std::cout << "Exception thrown on token (" << token << ")." << std::endl;
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
        { return tolower(c); } /* */
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
    Assembler::tokenToLowerCase(token);
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
        Assembler::willThrowOnToken(token);
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNKNOWN_DIRECTIVE);
    }

    return directive;
}

bool Assembler::isValidSymbolName(Assembler::token_t &token)
{
    const std::regex identTemplate("[a-zA-Z_][a-zA-Z0-9_]*");

    return std::regex_match(token, identTemplate);
}

ASM::word_t Assembler::tryParseLiteral(Assembler::token_t &token, int lineNumber)
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
            else if (token[i] - 'A' < 6)
            {
                value |= ((token[i] - 'A') + 10) << (4 * j);
            }
            else
            {
                value |= ((token[i] - 'a') + 10) << (4 * j);
            }
        }
    }
    else
    {
        Assembler::willThrowOnToken(token);
        throw Assembler::ErrorCode::INVALID_LITERAL;
    }

    return value;
}

bool Assembler::isDirectiveDeclaration(Assembler::token_t &token)
{
    return token[0] == ASM::ReservedSymbols[ASM::Symbol::DOT];
}

ASM::Regs Assembler::getRegisterCode(Assembler::token_t &token)
{
    Assembler::tokenToLowerCase(token);
    int regCode = ASM::Regs::INVALID_REG, i = 0;

    for (const auto &reg : ASM::Register)
    {
        if (token == reg)
        {
            regCode = i;
            break;
        }
        i++;
    }

    return static_cast<ASM::Regs>(regCode);
}

/* assembler semantic operations */
void Assembler::assertValidLabelDeclarationOn(int lineNumber, Assembler::token_t &token, Assembler::section_name_t &section)
{
    if (!Assembler::isValidSymbolName(token))
    {
        Assembler::willThrowOnToken(token);
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
    }

    Assembler::assertSectionPreviouslyDeclared(section, lineNumber);
}

ASM::Instr Assembler::assertValidInstruction(token_t &token, int lineNumber)
{
    ASM::Instr instrID = Assembler::isValidInstruction(token);

    if (instrID == ASM::Instr::INVALID_INSTR)
    {
        Assembler::willThrowOnToken(token);
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNKNOWN_INSTRUCTION);
    }

    return instrID;
}

void Assembler::assertValidInstructionCall(ASM::Instr instr, Assembler::section_name_t &section, int operandCount, int lineNumber)
{
    Assembler::assertSectionPreviouslyDeclared(section, lineNumber);

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
            Assembler::willThrowOnToken(tokens[labelOffset + 1]);
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
                Assembler::tryParseLiteral(tokens.at(i), lineNumber);
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
            Assembler::willThrowOnToken(tokens[labelOffset + 1]);
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

void Assembler::assertSectionPreviouslyDeclared(Assembler::section_name_t &section, int lineNumber)
{
    if (section == Assembler::Section::NO_SECTION)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::NO_SECTION);
    }
}

int Assembler::calcInstrOperandCountFromLine(Assembler::token_container_t &tokens, int labelOffset)
{
    return tokens.size() - 1 - labelOffset;
}

ASM::Regs Assembler::assertValidRegisterReference(Assembler::token_t &token, int lineNumber)
{
    bool regFound = false;
    int i = 0, regID = -1;

    for (const auto &reg : ASM::Register)
    {
        if (token == reg)
        {
            regFound = true;
            regID = i;
            break;
        }
        i++;
    }

    if (!regFound)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::NOT_ALL_REG);
    }

    return static_cast<ASM::Regs>((regFound) ? regID : ASM::Regs::INVALID_REG);
}

void Assembler::promoteSymbolToGlobal(Assembler::symbol_name_t &symbol)
{
    auto &row = *(SymbolTable.find(symbol));
    row.second.isLocal = false;
}

bool Assembler::isSymbolInSectionScope(Assembler::symbol_name_t &symbol, Assembler::section_name_t &section)
{
    auto &row = *(Assembler::SymbolTable.find(symbol));
    return row.second.section == section;
}

bool Assembler::isSymbolDeclared(Assembler::symbol_name_t &symbol)
{
    return Assembler::SymbolTable.count(symbol) > 0;
}

int Assembler::getSymbolOffset(Assembler::symbol_name_t &symbol)
{
    auto &row = *(Assembler::SymbolTable.find(symbol));
    return row.second.offset;
}

int Assembler::getSymbolOrderID(Assembler::symbol_name_t &symbol)
{
    auto &row = *(Assembler::SymbolTable.find(symbol));
    return row.second.orderID;
}

bytecode_t Assembler::parseBytecodeToAddress(bytecode_t bytecode)
{
    bytecode_t addr = bytecode.substr(3, 2);
    addr.push_back(bytecode[0]);
    addr.push_back(bytecode[1]);

    return addr;
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

void Assembler::createSectionTable(Assembler::section_name_t &section)
{
    section_table_pair_t sectionTable = {
        section,
        {0, std::vector<Assembler::SectionTableRow_t>()} /* */
    };
    Assembler::SectionTables.insert(sectionTable);
}

int Assembler::getSectionLocationCounter(Assembler::section_name_t &section)
{
    auto &sectionTable = *(Assembler::SectionTables.find(section));
    return sectionTable.second.locationCounter;
}

void Assembler::updateSectionLocationCounter(Assembler::section_name_t &section, int addByteCount)
{
    auto &sectionTable = *(Assembler::SectionTables.find(section));
    sectionTable.second.locationCounter += addByteCount;
}

void Assembler::insertIntoSectionTable(Assembler::section_name_t &section, Assembler::SectionTableRow_t row)
{
    auto &sectionTable = *(Assembler::SectionTables.find(section));

    sectionTable.second.row.push_back(row);
    Assembler::updateSectionLocationCounter(section, row.size);
}

void Assembler::initBytesWithZero(section_name_t &section, int byteCount)
{
    std::string bytecode = "";

    for (int i = 0; i < byteCount; i++)
    {
        bytecode += "00";
        if (i + 1 < byteCount)
        {
            bytecode.push_back(' ');
        }
    }

    int locCounter = Assembler::getSectionLocationCounter(section);
    Assembler::SectionTableRow_t row = {
        locCounter,
        byteCount,
        bytecode,
        "# Unused bytes initialized to 0 with .skip directive;" /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::assertSymbolUndeclared(Assembler::symbol_name_t &symbol, int lineNumber)
{
    if (Assembler::isSymbolDeclared(symbol))
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::DUPLICATE_IDENTIFIER);
    }
}

void Assembler::pushToSymbolTable(Assembler::symbol_table_row_pair_t row)
{
    Assembler::SymbolTableRow::ORDER_COUNTER++;
    Assembler::SymbolTable.insert(row);
}

void Assembler::insertAbsoluteSymbol(symbol_name_t &symbol, ASM::word_t literal, int lineNumber)
{
    Assembler::assertSymbolUndeclared(symbol, lineNumber);

    int locCounter = Assembler::getSectionLocationCounter(Assembler::Section::ABSOLUTE);

    Assembler::symbol_table_row_pair_t row = {
        symbol,
        {Assembler::Section::ABSOLUTE,
         locCounter,
         true,
         Assembler::SymbolTableRow::ORDER_COUNTER} /* */
    };
    Assembler::pushToSymbolTable(row);

    std::string desc;
    std::stringstream converter;
    converter << "# Local named literal " << symbol << " = " << literal << ";";
    desc = converter.str();

    Assembler::SectionTableRow_t sectionRow = {
        locCounter,
        ASM::InstrSize::WORD,
        Assembler::encodeLiteralToBytecode(literal),
        desc /* */
    };
    Assembler::insertIntoSectionTable(Assembler::Section::ABSOLUTE, sectionRow);
}

void Assembler::insertSection(Assembler::section_name_t &section, int lineNumber)
{
    Assembler::assertSymbolUndeclared(section, lineNumber);

    Assembler::symbol_table_row_pair_t symbolTableRow = {
        section,
        {section, 0, true, Assembler::SymbolTableRow::ORDER_COUNTER} /* */
    };
    Assembler::pushToSymbolTable(symbolTableRow);

    Assembler::createSectionTable(section);
}

void Assembler::insertLabel(Assembler::symbol_name_t &label, section_name_t &section, int lineNumber)
{
    Assembler::assertSymbolUndeclared(label, lineNumber);

    int locCounter = Assembler::getSectionLocationCounter(section);

    Assembler::symbol_table_row_pair_t symbolTableRow = {
        label,
        {section, locCounter, true, Assembler::SymbolTableRow::ORDER_COUNTER} /* */
    };
    Assembler::pushToSymbolTable(symbolTableRow);
}

int Assembler::insertExternalSymbol(Assembler::symbol_name_t &symbol)
{
    int orderID = Assembler::SymbolTableRow::ORDER_COUNTER;

    Assembler::symbol_table_row_pair_t symbolTableRow = {
        symbol,
        {Assembler::Section::EXTERNAL,
         Assembler::Offset::UNKNOWN,
         false,
         orderID} /* */
    };
    Assembler::pushToSymbolTable(symbolTableRow);

    return orderID;
}

int Assembler::prepareForNextLine(Assembler::token_container_t &tokens, int lineCounter)
{
    tokens.clear();
    return ++lineCounter;
}

void Assembler::storeLine(Assembler::lines_t &program, Assembler::token_container_t &tokens, int lineNumber, Assembler::LineTag tag)
{
    Assembler::Line_t line = {tokens, lineNumber, tag};
    program.push_back(line);
}

void Assembler::patchLineTag(Assembler::lines_t &program, Assembler::LineTag tag)
{
    auto &line = program.at(program.size() - 1);
    line.tag = tag;
}

void Assembler::resetSectionLocationCounters()
{
    for (auto &table : Assembler::SectionTables)
    {
        table.second.locationCounter = 0;
    }
}

void Assembler::initBytesWithLiteral(Assembler::token_t &token, Assembler::section_name_t &section, int lineNumber)
{
    ASM::word_t literal = Assembler::tryParseLiteral(token, lineNumber);
    std::string bytecode = Assembler::encodeLiteralToBytecode(literal);

    Assembler::assertSectionPreviouslyDeclared(section, lineNumber);

    int locCounter = Assembler::getSectionLocationCounter(section);
    std::stringstream stream;
    stream << "# Initialized directly with .word directive to value " << literal << ";";

    Assembler::SectionTableRow_t row = {
        locCounter,
        ASM::InstrSize::WORD,
        bytecode,
        stream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::initBytesWithSymbol(Assembler::token_t &token, Assembler::section_name_t &section, int lineNumber)
{
    int locCounter = Assembler::getSectionLocationCounter(section);
    Assembler::SectionTableRow_t sectionRow = {
        locCounter,
        ASM::InstrSize::WORD,
        "",
        "" /* */
    };

    if (!Assembler::isSymbolDeclared(token))
    {
        sectionRow.bytecode = "?? ??";
        sectionRow.description = "# .word symbol initialization with an external symbol, awaiting linker to patch this memory space;";
    }
    else
    {
        auto &row = *(Assembler::SymbolTable.find(token));

        ASM::word_t literal = (ASM::word_t)row.second.offset;
        sectionRow.bytecode = Assembler::encodeLiteralToBytecode(literal);

        std::stringstream stream;
        stream << "# .word initialization via symbol " << token << " with value " << literal << ".";
        sectionRow.description = stream.str();
    }

    Assembler::insertIntoSectionTable(section, sectionRow);
}

void Assembler::pushToRelocationTable(Assembler::RelocationTableRow_t row)
{
    Assembler::RelocationTable.push_back(row);
}

/* second pass directive processing */
void Assembler::processExternDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    for (int i = 1 + labelOffset; i < tokens.size(); i++)
    {
        if (!Assembler::isValidSymbolName(tokens[i]))
        {
            Assembler::willThrowOnToken(tokens[i]);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
        }

        if (Assembler::isSymbolDeclared(tokens[i]))
        {
            Assembler::willThrowOnToken(tokens[i]);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::REDECLARED_EXTERN);
        }
    }
}

void Assembler::processGlobalDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    for (int i = 1 + labelOffset; i < tokens.size(); i++)
    {
        if (!Assembler::isValidSymbolName(tokens[i]))
        {
            Assembler::willThrowOnToken(tokens[i]);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_IDENTIFIER);
        }

        if (!Assembler::isSymbolDeclared(tokens[i]))
        {
            Assembler::willThrowOnToken(tokens[i]);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::UNDECLARED_GLOBAL);
        }

        promoteSymbolToGlobal(tokens[i]);
    }
}

void Assembler::processSectionDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    section = tokens[1 + labelOffset];
}

bool Assembler::endDirectiveDetected(Assembler::token_container_t &tokens, int labelOffset, int lineNumber)
{
    ASM::Directive dir = Assembler::assertValidDirective(tokens[labelOffset], lineNumber);

    return dir == ASM::Directive::DOT_END;
}

void Assembler::assertProgramHasEnd(bool endDirectiveDetected, int lineNumber)
{
    if (!endDirectiveDetected)
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::NO_END_DIRECTIVE);
    }
}

void Assembler::processWordDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    ASM::word_t literal;
    Assembler::token_t token;
    std::string bytecode;

    for (int i = 1 + labelOffset; i < tokens.size(); i++)
    {
        token = tokens.at(i);

        if (Assembler::isValidSymbolName(token))
        {
            Assembler::initBytesWithSymbol(token, section, lineNumber);
        }
        else
        {
            Assembler::initBytesWithLiteral(token, section, lineNumber);
        }
    }
}

void Assembler::processSkipDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    int literal = Assembler::tryParseLiteral(tokens[labelOffset + 1], lineNumber);
    Assembler::initBytesWithZero(section, literal);
}

void Assembler::processDirective(Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    ASM::Directive dir = Assembler::assertValidDirective(tokens[labelOffset], lineNumber);

    if (dir == ASM::Directive::DOT_END)
    {
        // finish second pass on .end directive
        return;
    }

    switch (dir)
    {
    case ASM::Directive::DOT_EXTERN:
        Assembler::processExternDirective(tokens, labelOffset, lineNumber);
        break;

    case ASM::Directive::DOT_GLOBAL:
        Assembler::processGlobalDirective(tokens, labelOffset, lineNumber);
        break;

    case ASM::Directive::DOT_SECTION:
        Assembler::processSectionDirective(tokens, labelOffset, lineNumber, section);
        break;

    case ASM::Directive::DOT_WORD:
        Assembler::processWordDirective(tokens, labelOffset, lineNumber, section);
        break;

    case ASM::Directive::DOT_SKIP:
        Assembler::processSkipDirective(tokens, labelOffset, lineNumber, section);
        break;

    case ASM::Directive::DOT_EQU:
        // directive already fully processed in the first pass
        break;
    }
}

/* second pass operand processing */
ASM::AddrMode Assembler::determineJumpAddrMode(Assembler::operand_t &operand)
{
    char leadingChar = operand[0];
    const char percent = ASM::ReservedSymbols[ASM::Symbol::PERCENT],
               asterisk = ASM::ReservedSymbols[ASM::Symbol::STAR],
               bracketOpen = ASM::ReservedSymbols[ASM::Symbol::BRACKET_OPEN],
               bracketClose = ASM::ReservedSymbols[ASM::Symbol::BRACKET_CLOSE],
               plus = ASM::ReservedSymbols[ASM::Symbol::PLUS];

    ASM::AddrMode addrMode;

    if (leadingChar == asterisk)
    {
        leadingChar = operand[1];

        if (leadingChar == bracketOpen)
        {
            addrMode = (operand.find_first_of(plus) < 0) ? ASM::AddrMode::REG_INDIR : ASM::AddrMode::REG_INDIR_W_OFF;
        }
        else
        {
            addrMode = (Assembler::isValidRegisterReference(operand)) ? ASM::AddrMode::REG_DIR : ASM::AddrMode::MEM_DIR;
        }
    }
    else
    {
        addrMode = (leadingChar == percent) ? ASM::AddrMode::REG_DIR_W_OFF : ASM::AddrMode::IMM;
    }

    return addrMode;
}

/* second pass instruction processing */
void Assembler::processSingleByteInstruction(ASM::Instr instr, Assembler::section_name_t &section)
{
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    int locCounter = Assembler::getSectionLocationCounter(section);

    Assembler::SectionTableRow_t row = {
        locCounter,
        instrDesc.size,
        ASM::InstrCode[instr],
        ASM::Instruction[instr] /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processSingleRegOperandInstruction(ASM::Instr instr, Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    int locCounter = Assembler::getSectionLocationCounter(section);

    Assembler::token_t token = tokens[1 + labelOffset];
    Assembler::tokenToLowerCase(token);
    ASM::Regs regRef = Assembler::assertValidRegisterReference(token, lineNumber);

    std::stringstream bytecodeStream, descStream;
    if (instr == ASM::Instr::INT)
    {
        bytecodeStream << ASM::RegCode[regRef] << "F " << ASM::InstrCode[instr];
    }
    else if (instr == ASM::Instr::NOT)
    {
        bytecodeStream << ASM::RegCode[regRef] << ASM::RegCode[regRef] << " " << ASM::InstrCode[instr];
    }
    descStream << ASM::Instruction[instr] << " " << token << ";";

    Assembler::SectionTableRow_t row = {
        locCounter,
        instrDesc.size,
        bytecodeStream.str(),
        descStream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processDoubleRegOperandInstruction(ASM::Instr instr, Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    int locCounter = Assembler::getSectionLocationCounter(section);

    Assembler::token_t regDest = tokens[1 + labelOffset], regSrc = tokens[2 + labelOffset];
    Assembler::tokenToLowerCase(regDest);
    Assembler::tokenToLowerCase(regSrc);
    ASM::Regs regDestRef = Assembler::assertValidRegisterReference(regDest, lineNumber);
    ASM::Regs regSrcRef = Assembler::assertValidRegisterReference(regSrc, lineNumber);

    std::stringstream bytecodeStream, descStream;
    bytecodeStream << ASM::RegCode[regDestRef] << ASM::RegCode[regSrcRef] << " " << ASM::InstrCode[instr];
    descStream << ASM::Instruction[instr] << " " << regDest << ", " << regSrc << ";";

    Assembler::SectionTableRow_t row = {
        locCounter,
        instrDesc.size,
        bytecodeStream.str(),
        descStream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processJumpInstruction(ASM::Instr instr, Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    // this one is a huge mess, cba to refactor, good luck
    // process the jump instructions with its various operand syntax formats
    int locCounter = Assembler::getSectionLocationCounter(section);
    bool useAltSize = false;

    bytecode_t instrCode = ASM::InstrCode[instr];
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    Assembler::token_t operand = tokens[1 + labelOffset];
    bytecode_t pc = ASM::RegCode[ASM::Regs::PC];

    ASM::AddrMode addrMode;
    bytecode_t addrModeCode;

    std::stringstream bytecodeStream, descStream;
    descStream << ASM::Instruction[instr] << " ";

    char leadingChar = operand[0];
    // handle *<...>
    if (leadingChar == ASM::ReservedSymbols[ASM::Symbol::STAR])
    {
        operand.erase(0, 1);

        // handle *<literal|symbol|reg...>
        if (operand[0] != ASM::ReservedSymbols[ASM::Symbol::BRACKET_OPEN])
        {
            if (!Assembler::isValidSymbolName(operand))
            {
                try
                {
                    // handle *<literal>
                    ASM::word_t literal = Assembler::tryParseLiteral(operand, lineNumber);
                    bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                    bytecode_t jumpAddr = Assembler::parseBytecodeToAddress(value);

                    useAltSize = true;
                    addrMode = ASM::AddrMode::MEM_DIR;
                    addrModeCode = ASM::AddrModeCode[addrMode];

                    bytecodeStream << value << " " << addrModeCode << " F" << pc << " " << instrCode;
                    descStream << "mem[" << jumpAddr << "] \t# memory indirect jump;";
                }
                catch (Assembler::ErrorCode code)
                {
                    Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
                }
            }
            else if (Assembler::isValidRegisterReference(operand))
            {
                // handle *<reg>
                ASM::Regs regCode = Assembler::getRegisterCode(operand);
                bytecode_t reg = ASM::RegCode[regCode];

                useAltSize = false;
                addrMode = ASM::AddrMode::REG_DIR;
                addrModeCode = ASM::AddrModeCode[addrMode];

                bytecodeStream << addrModeCode << " F" << reg << " " << instrCode;
                descStream << ASM::Register[regCode] << " \t# registry direct jump;";
            }
            else
            {
                // handle *<symbol>
                useAltSize = true;
                addrMode = ASM::AddrMode::MEM_DIR;
                addrModeCode = ASM::AddrModeCode[addrMode];

                if (Assembler::isSymbolDeclared(operand))
                {
                    if (Assembler::isSymbolInSectionScope(operand, section))
                    {
                        // handle *<symbol>, symbol in section
                        int symbolOffset = Assembler::getSymbolOffset(operand);
                        int nextInstr = locCounter + instrDesc.altSize;

                        // perform correct truncation down to 16bit for signed operands
                        int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                        ASM::word_t payload = diff_32b;
                        payload = (symbolOffset > nextInstr) ? payload : -payload;
                        bytecode_t value = Assembler::encodeLiteralToBytecode(payload);
                        bytecode_t payloadAddr = Assembler::parseBytecodeToAddress(value);

                        bytecodeStream << value << " " << addrModeCode << " F" << pc << " " << instrCode;
                        descStream << "mem[%pc+0x" << payloadAddr << "] (" << operand << ") \t# memory indirect jump;";
                    }
                    else
                    {
                        // handle *<symbol>, symbol in another section
                        int orderID = Assembler::getSymbolOrderID(operand);
                        bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                        Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                        Assembler::RelocationTableRow_t relocRow = {
                            section,
                            locCounter,
                            relocType,
                            orderID,
                            0 /* */
                        };
                        Assembler::pushToRelocationTable(relocRow);

                        bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
                        descStream << "mem[0x????] (" << operand << ") \t# memory indirect jump via symbol in another section, waiting for linker to patch this memory space;" << std::endl;
                        descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
                    }
                }
                else
                {
                    // handle *<symbol>, symbol is external
                    int orderID = Assembler::insertExternalSymbol(operand);
                    int nextInstr = locCounter + instrDesc.altSize;
                    bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                    Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                    Assembler::RelocationTableRow_t relocRow = {
                        section,
                        locCounter,
                        relocType,
                        orderID,
                        -instrDesc.altSize /* */
                    };
                    Assembler::pushToRelocationTable(relocRow);

                    bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
                    descStream << "mem[0x????] (" << operand << ") \t# memory indirect jump via external symbol, waiting for linker to patch this memory space;" << std::endl;
                    descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
                }
            }
        }
        else if (operand[operand.length() - 1] != ASM::ReservedSymbols[ASM::Symbol::BRACKET_CLOSE])
        {
            Assembler::willThrowOnToken(operand);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
        }
        else
        {
            // handle *[reg...]
            int plusSymbolIndex = operand.find_first_of(ASM::ReservedSymbols[ASM::Symbol::PLUS]);
            int operandLast = operand.length() - 1;

            if (plusSymbolIndex >= 0)
            {
                // handle *[reg + ...]
                token_t reg = operand.substr(1, plusSymbolIndex - 1);
                token_t payload = operand.substr(plusSymbolIndex + 1, operandLast - plusSymbolIndex - 1);

                if (!Assembler::isValidRegisterReference(reg))
                {
                    Assembler::willThrowOnToken(operand);
                    Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
                }

                ASM::Regs regCode = Assembler::getRegisterCode(reg);

                useAltSize = true;
                addrMode = ASM::AddrMode::REG_INDIR_W_OFF;
                addrModeCode = ASM::AddrModeCode[addrMode];

                // handle *[reg + symbol]
                if (Assembler::isValidSymbolName(payload))
                {
                    if (Assembler::isSymbolDeclared(payload))
                    {
                        if (Assembler::isSymbolInSectionScope(payload, section))
                        {
                            // handle *[reg + symbol], symbol in section
                            int symbolOffset = Assembler::getSymbolOffset(payload);
                            int nextInstr = locCounter + instrDesc.altSize;

                            // perform correct truncation down to 16bit for signed operands
                            int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                            ASM::word_t diff = diff_32b;
                            diff = (symbolOffset > nextInstr) ? diff : -diff;
                            bytecode_t value = Assembler::encodeLiteralToBytecode(diff);
                            bytecode_t valueAddress = Assembler::parseBytecodeToAddress(value);

                            bytecodeStream << value << " " << addrModeCode << " " << ASM::RegCode[regCode] << pc << " " << instrCode;
                            descStream << "mem[" << ASM::Register[regCode] << "+0x" << valueAddress << "] (" << payload << ") \t# registry indirect with offset jump;";
                        }
                        else
                        {
                            // handle *[reg + symbol], symbol in another section
                            int orderID = Assembler::getSymbolOrderID(payload);
                            bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                            Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                            Assembler::RelocationTableRow_t relocRow = {
                                section,
                                locCounter,
                                relocType,
                                orderID,
                                0 /* */
                            };
                            Assembler::pushToRelocationTable(relocRow);

                            bytecodeStream << "?? ?? " << addrModeCode << " " << ASM::RegCode[regCode] << pc << " " << instrCode;
                            descStream << "mem[" << ASM::Register[regCode] << "+0x????] (" << payload << ") \t# registry indirect with offset jump via symbol offset from another section, waiting for linker to patch this memory space;" << std::endl;
                            descStream << "\t" << relocOffset << ": " << relocType << "\t" << payload << "-0x0004;";
                        }
                    }
                    else
                    {
                        // handle *[reg + symbol], symbol is external
                        int orderID = Assembler::insertExternalSymbol(payload);
                        int nextInstr = locCounter + instrDesc.altSize;
                        bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                        Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                        Assembler::RelocationTableRow_t relocRow = {
                            section,
                            locCounter,
                            relocType,
                            orderID,
                            -instrDesc.altSize /* */
                        };
                        Assembler::pushToRelocationTable(relocRow);

                        bytecodeStream << "?? ?? " << addrModeCode << " " << ASM::RegCode[regCode] << pc << " " << instrCode;
                        descStream << "mem[" << ASM::Register[regCode] << "+0x????] (" << payload << ") \t# registry indirect with offset jump via external symbol offset, waiting for linker to patch this memory space;" << std::endl;
                        descStream << "\t" << relocOffset << ": " << relocType << "\t" << payload << "-0x0004;";
                    }
                }
                else
                {
                    // handle *[reg + literal]
                    try
                    {
                        ASM::word_t literal = Assembler::tryParseLiteral(payload, lineNumber);
                        bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                        bytecode_t payloadBytecode = Assembler::parseBytecodeToAddress(value);

                        useAltSize = true;
                        addrMode = ASM::AddrMode::REG_INDIR_W_OFF;
                        addrModeCode = ASM::AddrModeCode[addrMode];

                        bytecodeStream << value << " " << addrModeCode << " " << ASM::RegCode[regCode] << pc << " " << instrCode;
                        descStream << "mem[" << ASM::Register[regCode] << "+0x" << payloadBytecode << "] \t# registry indirect with offset jump;";
                    }
                    catch (Assembler::ErrorCode code)
                    {
                        Assembler::willThrowOnToken(operand);
                        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
                    }
                }
            }
            else
            {
                // handle *[reg]
                token_t reg = operand.substr(1, operand.length() - 2);

                if (!Assembler::isValidRegisterReference(reg))
                {
                    Assembler::willThrowOnToken(operand);
                    Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
                }

                ASM::Regs regCode = Assembler::getRegisterCode(reg);
                bytecode_t regBytecode = ASM::RegCode[regCode];

                useAltSize = false;
                addrMode = ASM::AddrMode::REG_INDIR;
                addrModeCode = ASM::AddrModeCode[addrMode];

                bytecodeStream << addrModeCode << " F" << regBytecode << " " << instrCode;
                descStream << "mem[" << ASM::Register[regCode] << "] \t # registry indirect jump;";
            }
        }
    }
    else if (leadingChar == ASM::ReservedSymbols[ASM::Symbol::PERCENT])
    {
        // handle %<symbol>
        operand.erase(0, 1);
        if (!Assembler::isValidSymbolName(operand))
        {
            Assembler::willThrowOnToken(operand);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
        }

        useAltSize = true;
        addrMode = ASM::AddrMode::REG_DIR_W_OFF;
        addrModeCode = ASM::AddrModeCode[addrMode];

        // handle %<symbol>
        if (Assembler::isSymbolDeclared(operand))
        {
            if (Assembler::isSymbolInSectionScope(operand, section))
            {
                // handle %<symbol>, symbol in section
                int symbolOffset = Assembler::getSymbolOffset(operand);
                bytecode_t value = Assembler::encodeLiteralToBytecode(symbolOffset);
                bytecode_t payloadAddr = Assembler::parseBytecodeToAddress(value);

                bytecodeStream << value << " " << addrModeCode << " F" << pc << " " << instrCode;
                descStream << "<%pc+(" << section << "+0x" << payloadAddr << ")> (" << operand << ") \t# pc relative jump;";
            }
            else
            {
                // handle %<symbol>, symbol in another section
                int orderID = Assembler::getSymbolOrderID(operand);
                bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                Assembler::RelocationTableRow_t relocRow = {
                    section,
                    locCounter,
                    relocType,
                    orderID,
                    0 /* */
                };
                Assembler::pushToRelocationTable(relocRow);

                bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
                descStream << "<%pc+0x??\?\?> (" << operand << ") \t# pc relative jump via symbol offset in another section, waiting for linker to patch this memory space;" << std::endl;
                descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
            }
        }
        else
        {
            // handle %<symbol>, symbol is external
            int orderID = Assembler::insertExternalSymbol(operand);
            int nextInstr = locCounter + instrDesc.altSize;
            bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
            Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

            Assembler::RelocationTableRow_t relocRow = {
                section,
                locCounter,
                relocType,
                orderID,
                -instrDesc.altSize /* */
            };
            Assembler::pushToRelocationTable(relocRow);

            bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
            descStream << "<%pc+0x??\?\?> (" << operand << ") \t# pc relative jump via external symbol offset, waiting for linker to patch this memory space;" << std::endl;
            descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
        }
    }
    else
    {
        // handle <symbol|literal>
        useAltSize = true;
        addrMode = ASM::AddrMode::IMM;
        addrModeCode = ASM::AddrModeCode[addrMode];

        // handle <symbol|literal>
        if (Assembler::isValidSymbolName(operand))
        {
            if (Assembler::isSymbolDeclared(operand))
            {
                if (Assembler::isSymbolInSectionScope(operand, section))
                {
                    // handle <symbol>, symbol in section
                    int symbolOffset = Assembler::getSymbolOffset(operand);
                    int nextInstr = locCounter + instrDesc.altSize;

                    // perform correct truncation down to 16bit for signed operands
                    int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                    ASM::word_t payload = diff_32b;
                    payload = (symbolOffset > nextInstr) ? payload : -payload;
                    bytecode_t value = Assembler::encodeLiteralToBytecode(payload);
                    bytecode_t valueAddr = Assembler::parseBytecodeToAddress(value);

                    bytecodeStream << value << " " << addrModeCode << " F" << pc << " " << instrCode;
                    descStream << "<%pc+0x" << valueAddr << "> (" << operand << ") \t# absolute jump;";
                }
                else
                {
                    // handle <symbol>, symbol in another section
                    int orderID = Assembler::getSymbolOrderID(operand);
                    bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                    Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                    Assembler::RelocationTableRow_t relocRow = {
                        section,
                        locCounter,
                        relocType,
                        orderID,
                        0 /* */
                    };
                    Assembler::pushToRelocationTable(relocRow);

                    bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
                    descStream << "0x???? (" << operand << ") \t# absolute jump via symbol in another section, waiting for linker to patch this memory space;" << std::endl;
                    descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
                }
            }
            else
            {
                // handle <symbol>, symbol is external
                int orderID = Assembler::insertExternalSymbol(operand);
                int nextInstr = locCounter + instrDesc.altSize;
                bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                Assembler::RelocationTableRow_t relocRow = {
                    section,
                    locCounter,
                    relocType,
                    orderID,
                    -instrDesc.altSize /* */
                };
                Assembler::pushToRelocationTable(relocRow);

                bytecodeStream << "?? ?? " << addrModeCode << " F" << pc << " " << instrCode;
                descStream << "0x???? (" << operand << ") \t# absolute jump via external symbol, waiting for linker to patch this memory space;" << std::endl;
                descStream << "\t" << relocOffset << ": " << relocType << "\t" << operand << "-0x0004;";
            }
        }
        else
        {
            // handle <literal>
            try
            {
                ASM::word_t literal = Assembler::tryParseLiteral(operand, lineNumber);
                bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                bytecode_t jumpAddr = Assembler::parseBytecodeToAddress(value);

                bytecodeStream << value << " " << addrModeCode << " F" << pc << " " << instrCode;
                descStream << "0x" << jumpAddr << " # absolute jump;";
            }
            catch (Assembler::ErrorCode code)
            {
                Assembler::willThrowOnToken(operand);
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_JUMP_OPERAND);
            }
        }
    }

    Assembler::SectionTableRow_t row = {
        locCounter,
        (useAltSize) ? instrDesc.altSize : instrDesc.size,
        bytecodeStream.str(),
        descStream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processPushPop(ASM::Instr instr, Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    // transpile into valid instructions:
    // push <reg> => str <reg>, [sp] (sp -= 2 before);
    // pop <reg> => ldr <reg>, [sp] (sp += 2 after);

    int locCounter = Assembler::getSectionLocationCounter(section);

    bytecode_t instrCode = ASM::InstrCode[(instr == ASM::Instr::PUSH) ? ASM::Instr::STR : ASM::Instr::LDR];
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];
    Assembler::token_t operand = tokens[1 + labelOffset];
    ASM::Regs sp = ASM::Regs::SP;

    ASM::AddrMode addrMode;
    std::stringstream bytecodeStream, descStream;

    Assembler::assertValidRegisterReference(operand, lineNumber);

    ASM::Regs reg = Assembler::getRegisterCode(operand);

    //std::cout << "\n\nDEBUG: " << instr << ", " << instrCode << "\n\n";

    if (instr == ASM::Instr::PUSH)
    {
        addrMode = ASM::AddrMode::REG_INDIR_DEC_BEFORE;
        bytecodeStream << ASM::AddrModeCode[addrMode] << " " << ASM::RegCode[reg] << ASM::RegCode[sp] << " " << instrCode;
        descStream << ASM::Instruction[instr] << " " << ASM::Register[reg];
    }
    else if (instr == ASM::Instr::POP)
    {
        addrMode = ASM::AddrMode::REG_INDIR_INC_AFTER;
        bytecodeStream << ASM::AddrModeCode[addrMode] << " " << ASM::RegCode[reg] << ASM::RegCode[sp] << " " << instrCode;
        descStream << ASM::Instruction[instr] << " " << ASM::Register[reg];
    }
    else
    {
        return;
    }

    Assembler::SectionTableRow_t row = {
        locCounter,
        instrDesc.size,
        bytecodeStream.str(),
        descStream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processLoadStore(ASM::Instr instr, Assembler::token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    // yep, even more garbage
    // processes the ldr/str instructions with its various operand syntax formats

    int locCounter = Assembler::getSectionLocationCounter(section);
    bool useAltSize = false;

    bytecode_t instrCode = ASM::InstrCode[instr];
    ASM::InstrDescTable_t instrDesc = ASM::InstructionDescTable[instr];

    Assembler::token_t firstOp = tokens[1 + labelOffset], secondOp = tokens[2 + labelOffset];

    ASM::AddrMode addrMode;
    bytecode_t addrModeCode;
    ASM::Regs firstOpCode;
    bytecode_t firstOpBytecode;

    std::stringstream bytecodeStream, descStream;

    if (!Assembler::isValidRegisterReference(firstOp))
    {
        Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_FIRST_OPERAND);
    }

    firstOpCode = Assembler::getRegisterCode(firstOp);
    firstOpBytecode = ASM::RegCode[firstOpCode];
    descStream << ASM::Instruction[instr] << " " << ASM::Register[firstOpCode] << ", ";

    char leadingChar = secondOp[0];

    // handle $<symbol|literal>
    if (leadingChar == ASM::ReservedSymbols[ASM::Symbol::DOLLAR])
    {
        secondOp.erase(0, 1);

        useAltSize = true;
        addrMode = ASM::AddrMode::IMM;
        addrModeCode = ASM::AddrModeCode[addrMode];

        // handle $<symbol|literal>
        if (Assembler::isValidSymbolName(secondOp))
        {
            if (Assembler::isSymbolDeclared(secondOp))
            {
                if (Assembler::isSymbolInSectionScope(secondOp, section))
                {
                    // handle <symbol>, symbol in section
                    int symbolOffset = Assembler::getSymbolOffset(secondOp);
                    int nextInstr = locCounter + instrDesc.altSize;

                    // perform correct truncation down to 16bit for signed operands
                    int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                    ASM::word_t payload = diff_32b;
                    payload = (symbolOffset > nextInstr) ? payload : -payload;
                    bytecode_t value = Assembler::encodeLiteralToBytecode(payload);
                    bytecode_t valueAddr = Assembler::parseBytecodeToAddress(value);

                    bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                    descStream << "<%pc+0x" << valueAddr << "> (" << secondOp << ") \t# immediate addressing;";
                }
                else
                {
                    // handle <symbol>, symbol in another section
                    int orderID = Assembler::getSymbolOrderID(secondOp);
                    bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                    Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                    Assembler::RelocationTableRow_t relocRow = {
                        section,
                        locCounter,
                        relocType,
                        orderID,
                        0 /* */
                    };
                    Assembler::pushToRelocationTable(relocRow);

                    bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                    descStream << "0x???? (" << secondOp << ") \t# immediate addressing via symbol in another section, waiting for linker to patch this memory space;" << std::endl;
                    descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
                }
            }
            else
            {
                // handle <symbol>, symbol is external
                int orderID = Assembler::insertExternalSymbol(secondOp);
                int nextInstr = locCounter + instrDesc.altSize;
                bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                Assembler::RelocationTableRow_t relocRow = {
                    section,
                    locCounter,
                    relocType,
                    orderID,
                    -instrDesc.altSize /* */
                };
                Assembler::pushToRelocationTable(relocRow);

                bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                descStream << "0x???? (" << secondOp << ") \t# immediate addressing via external symbol, waiting for linker to patch this memory space;" << std::endl;
                descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
            }
        }
        else
        {
            // handle <literal>
            try
            {
                ASM::word_t literal = Assembler::tryParseLiteral(secondOp, lineNumber);
                bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                bytecode_t valueFixed = Assembler::parseBytecodeToAddress(value);

                bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                descStream << "0x" << valueFixed << " # immediate addressing;";
            }
            catch (Assembler::ErrorCode code)
            {
                Assembler::willThrowOnToken(secondOp);
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
            }
        }
    }
    // handle %<symbol>
    else if (leadingChar == ASM::ReservedSymbols[ASM::Symbol::PERCENT])
    {
        secondOp.erase(0, 1);
        if (!Assembler::isValidSymbolName(secondOp))
        {
            Assembler::willThrowOnToken(secondOp);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
        }

        useAltSize = true;
        addrMode = ASM::AddrMode::REG_INDIR_W_OFF;
        addrModeCode = ASM::AddrModeCode[addrMode];

        // handle %<symbol>
        if (Assembler::isSymbolDeclared(secondOp))
        {
            if (Assembler::isSymbolInSectionScope(secondOp, section))
            {
                // handle %<symbol>, symbol in section
                int symbolOffset = Assembler::getSymbolOffset(secondOp);
                bytecode_t value = Assembler::encodeLiteralToBytecode(symbolOffset);
                bytecode_t payloadAddr = Assembler::parseBytecodeToAddress(value);

                bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[ASM::Regs::PC] << " " << instrCode;
                descStream << "mem[<%pc+(" << section << "+0x" << payloadAddr << ")>] (" << secondOp << ") \t# pc relative memory access;";
            }
            else
            {
                // handle %<symbol>, symbol in another section
                int orderID = Assembler::getSymbolOrderID(secondOp);
                bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                Assembler::RelocationTableRow_t relocRow = {
                    section,
                    locCounter,
                    relocType,
                    orderID,
                    0 /* */
                };
                Assembler::pushToRelocationTable(relocRow);

                bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[ASM::Regs::PC] << " " << instrCode;
                descStream << "mem[<%pc+0x??\?\?>] (" << secondOp << ") \t# pc relative memory access via symbol offset in another section, waiting for linker to patch this memory space;" << std::endl;
                descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
            }
        }
        else
        {
            // handle %<symbol>, symbol is external
            int orderID = Assembler::insertExternalSymbol(secondOp);
            int nextInstr = locCounter + instrDesc.altSize;
            bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
            Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

            Assembler::RelocationTableRow_t relocRow = {
                section,
                locCounter,
                relocType,
                orderID,
                -instrDesc.altSize /* */
            };
            Assembler::pushToRelocationTable(relocRow);

            bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[ASM::Regs::PC] << " " << instrCode;
            descStream << "mem[<%pc+0x??\?\?>] (" << secondOp << ") \t# pc relative memory access via external symbol offset, waiting for linker to patch this memory space;" << std::endl;
            descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
        }
    }
    // handle <[reg...]>
    else if (leadingChar == ASM::ReservedSymbols[ASM::Symbol::BRACKET_OPEN])
    {
        if (secondOp[secondOp.length() - 1] != ASM::ReservedSymbols[ASM::Symbol::BRACKET_CLOSE])
        {
            Assembler::willThrowOnToken(secondOp);
            Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
        }

        // handle [reg...]
        int plusSymbolIndex = secondOp.find_first_of(ASM::ReservedSymbols[ASM::Symbol::PLUS]);
        int operandLast = secondOp.length() - 1;

        if (plusSymbolIndex >= 0)
        {
            // handle [reg + ...]
            token_t reg = secondOp.substr(1, plusSymbolIndex - 1);
            token_t payload = secondOp.substr(plusSymbolIndex + 1, operandLast - plusSymbolIndex - 1);

            if (!Assembler::isValidRegisterReference(reg))
            {
                Assembler::willThrowOnToken(secondOp);
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
            }

            ASM::Regs regCode = Assembler::getRegisterCode(reg);

            useAltSize = true;
            addrMode = ASM::AddrMode::REG_INDIR_W_OFF;
            addrModeCode = ASM::AddrModeCode[addrMode];

            // handle *[reg + symbol]
            if (Assembler::isValidSymbolName(payload))
            {
                if (Assembler::isSymbolDeclared(payload))
                {
                    if (Assembler::isSymbolInSectionScope(payload, section))
                    {
                        // handle *[reg + symbol], symbol in section
                        int symbolOffset = Assembler::getSymbolOffset(payload);
                        int nextInstr = locCounter + instrDesc.altSize;

                        // perform correct truncation down to 16bit for signed operands
                        int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                        ASM::word_t diff = diff_32b;
                        diff = (symbolOffset > nextInstr) ? diff : -diff;
                        bytecode_t value = Assembler::encodeLiteralToBytecode(diff);
                        bytecode_t valueAddress = Assembler::parseBytecodeToAddress(value);

                        bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[regCode] << " " << instrCode;
                        descStream << "mem[" << ASM::Register[regCode] << "+0x" << valueAddress << "] (" << payload << ") \t# registry indirect with offset addressing;";
                    }
                    else
                    {
                        // handle *[reg + symbol], symbol in another section
                        int orderID = Assembler::getSymbolOrderID(payload);
                        bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                        Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                        Assembler::RelocationTableRow_t relocRow = {
                            section,
                            locCounter,
                            relocType,
                            orderID,
                            0 /* */
                        };
                        Assembler::pushToRelocationTable(relocRow);

                        bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[regCode] << " " << instrCode;
                        descStream << "mem[" << ASM::Register[regCode] << "+0x????] (" << payload << ") \t# registry indirect with offset addressing via symbol offset from another section, waiting for linker to patch this memory space;" << std::endl;
                        descStream << "\t" << relocOffset << ": " << relocType << "\t" << payload << "-0x0004;";
                    }
                }
                else
                {
                    // handle *[reg + symbol], symbol is external
                    int orderID = Assembler::insertExternalSymbol(payload);
                    int nextInstr = locCounter + instrDesc.altSize;
                    bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                    Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                    Assembler::RelocationTableRow_t relocRow = {
                        section,
                        locCounter,
                        relocType,
                        orderID,
                        -instrDesc.altSize /* */
                    };
                    Assembler::pushToRelocationTable(relocRow);

                    bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[regCode] << " " << instrCode;
                    descStream << "mem[" << ASM::Register[regCode] << "+0x????] (" << payload << ") \t# registry indirect with offset addressing via external symbol offset, waiting for linker to patch this memory space;" << std::endl;
                    descStream << "\t" << relocOffset << ": " << relocType << "\t" << payload << "-0x0004;";
                }
            }
            else
            {
                // handle [reg + literal]
                try
                {
                    ASM::word_t literal = Assembler::tryParseLiteral(payload, lineNumber);
                    bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                    bytecode_t payloadBytecode = Assembler::parseBytecodeToAddress(value);

                    useAltSize = true;
                    addrMode = ASM::AddrMode::REG_INDIR_W_OFF;
                    addrModeCode = ASM::AddrModeCode[addrMode];

                    bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << ASM::RegCode[regCode] << " " << instrCode;
                    descStream << "mem[" << ASM::Register[regCode] << "+0x" << payloadBytecode << "] \t# registry indirect with offset addressing;";
                }
                catch (Assembler::ErrorCode code)
                {
                    Assembler::willThrowOnToken(secondOp);
                    Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
                }
            }
        }
        else
        {
            // handle [reg]
            token_t reg = secondOp.substr(1, secondOp.length() - 2);

            if (!Assembler::isValidRegisterReference(reg))
            {
                Assembler::willThrowOnToken(secondOp);
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
            }

            ASM::Regs regCode = Assembler::getRegisterCode(reg);
            bytecode_t regBytecode = ASM::RegCode[regCode];

            useAltSize = false;
            addrMode = ASM::AddrMode::REG_INDIR;
            addrModeCode = ASM::AddrModeCode[addrMode];

            bytecodeStream << addrModeCode << " " << firstOpBytecode << regBytecode << " " << instrCode;
            descStream << "mem[" << ASM::Register[regCode] << "] \t # registry indirect addressing;";
        }
    }
    // handle <symbol|literal|reg>
    else
    {
        // handle <reg>
        if (Assembler::isValidRegisterReference(secondOp))
        {
            ASM::Regs secondRegCode = Assembler::getRegisterCode(secondOp);
            bytecode_t secondOpBytecode = ASM::RegCode[secondRegCode];

            useAltSize = false;
            addrMode = ASM::AddrMode::REG_DIR;
            addrModeCode = ASM::AddrModeCode[addrMode];

            bytecodeStream << addrModeCode << " " << firstOpBytecode << secondOpBytecode << " " << instrCode;
            descStream << ASM::Register[secondRegCode] << " \t # registry direct addressing;";
        }
        // handle <symbol>
        else if (Assembler::isValidSymbolName(secondOp))
        {
            useAltSize = true;
            addrMode = ASM::AddrMode::MEM_DIR;
            addrModeCode = ASM::AddrModeCode[addrMode];

            if (Assembler::isSymbolDeclared(secondOp))
            {
                if (Assembler::isSymbolInSectionScope(secondOp, section))
                {
                    // handle <symbol>, symbol in section
                    int symbolOffset = Assembler::getSymbolOffset(secondOp);
                    int nextInstr = locCounter + instrDesc.altSize;

                    // perform correct truncation down to 16bit for signed operands
                    int diff_32b = (symbolOffset > nextInstr) ? symbolOffset - nextInstr : nextInstr - symbolOffset;
                    ASM::word_t payload = diff_32b;
                    payload = (symbolOffset > nextInstr) ? payload : -payload;
                    bytecode_t value = Assembler::encodeLiteralToBytecode(payload);
                    bytecode_t valueAddr = Assembler::parseBytecodeToAddress(value);

                    bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                    descStream << "mem[<%pc+0x" << valueAddr << ">] (" << secondOp << ") \t# memory direct addressing;";
                }
                else
                {
                    // handle <symbol>, symbol in another section
                    int orderID = Assembler::getSymbolOrderID(secondOp);
                    bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                    Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_ABS;

                    Assembler::RelocationTableRow_t relocRow = {
                        section,
                        locCounter,
                        relocType,
                        orderID,
                        0 /* */
                    };
                    Assembler::pushToRelocationTable(relocRow);

                    bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                    descStream << "mem[0x????] (" << secondOp << ") \t# memory direct addressing via symbol in another section, waiting for linker to patch this memory space;" << std::endl;
                    descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
                }
            }
            else
            {
                // handle <symbol>, symbol is external
                int orderID = Assembler::insertExternalSymbol(secondOp);
                int nextInstr = locCounter + instrDesc.altSize;
                bytecode_t relocOffset = Assembler::parseBytecodeToAddress(Assembler::encodeLiteralToBytecode(locCounter + instrDesc.altSize + 1));
                Assembler::relocation_type_t relocType = Assembler::RelocationType::R_MOCK_CPU_PC16;

                Assembler::RelocationTableRow_t relocRow = {
                    section,
                    locCounter,
                    relocType,
                    orderID,
                    -instrDesc.altSize /* */
                };
                Assembler::pushToRelocationTable(relocRow);

                bytecodeStream << "?? ?? " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                descStream << "mem[0x????] (" << secondOp << ") \t# mmeory direct addressing via external symbol, waiting for linker to patch this memory space;" << std::endl;
                descStream << "\t" << relocOffset << ": " << relocType << "\t" << secondOp << "-0x0004;";
            }
        }
        // handle <literal>
        else
        {
            useAltSize = true;
            addrMode = ASM::AddrMode::MEM_DIR;
            addrModeCode = ASM::AddrModeCode[addrMode];

            try
            {
                ASM::word_t literal = Assembler::tryParseLiteral(secondOp, lineNumber);
                bytecode_t value = Assembler::encodeLiteralToBytecode(literal);
                bytecode_t valueAddr = Assembler::parseBytecodeToAddress(value);

                bytecodeStream << value << " " << addrModeCode << " " << firstOpBytecode << "F " << instrCode;
                descStream << "mem[0x" << valueAddr << "] # memory direct addressing;";
            }
            catch (Assembler::ErrorCode code)
            {
                Assembler::willThrowOnToken(secondOp);
                Assembler::reportErrorAtLineAndThrow(lineNumber, Assembler::ErrorCode::INVALID_SECOND_OPERAND);
            }
        }
    }

    Assembler::SectionTableRow_t row = {
        locCounter,
        (useAltSize) ? instrDesc.altSize : instrDesc.size,
        bytecodeStream.str(),
        descStream.str() /* */
    };
    Assembler::insertIntoSectionTable(section, row);
}

void Assembler::processInstruction(token_container_t &tokens, int labelOffset, int lineNumber, Assembler::section_name_t &section)
{
    Assembler::token_t token = tokens[labelOffset];
    ASM::Instr instr = Assembler::assertValidInstruction(token, lineNumber);

    int operandCount = Assembler::calcInstrOperandCountFromLine(tokens, labelOffset);
    Assembler::assertValidInstructionCall(instr, section, operandCount, lineNumber);

    switch (instr)
    {
    case ASM::Instr::HALT:
    case ASM::Instr::IRET:
    case ASM::Instr::RET:
        Assembler::processSingleByteInstruction(instr, section);
        break;
    case ASM::Instr::INT:
    case ASM::Instr::NOT:
        Assembler::processSingleRegOperandInstruction(instr, tokens, labelOffset, lineNumber, section);
        break;
    case ASM::Instr::XCHG:
    case ASM::Instr::ADD:
    case ASM::Instr::SUB:
    case ASM::Instr::MUL:
    case ASM::Instr::DIV:
    case ASM::Instr::CMP:
    case ASM::Instr::AND:
    case ASM::Instr::OR:
    case ASM::Instr::XOR:
    case ASM::Instr::TEST:
    case ASM::Instr::SHL:
    case ASM::Instr::SHR:
        Assembler::processDoubleRegOperandInstruction(instr, tokens, labelOffset, lineNumber, section);
        break;
    case ASM::Instr::JMP:
    case ASM::Instr::JEQ:
    case ASM::Instr::JNE:
    case ASM::Instr::JGT:
    case ASM::Instr::CALL:
        Assembler::processJumpInstruction(instr, tokens, labelOffset, lineNumber, section);
        break;
    case ASM::Instr::PUSH:
    case ASM::Instr::POP:
        Assembler::processPushPop(instr, tokens, labelOffset, lineNumber, section);
        break;
    case ASM::Instr::LDR:
    case ASM::Instr::STR:
        Assembler::processLoadStore(instr, tokens, labelOffset, lineNumber, section);
        break;
    default:
        break;
    }
}

/* assembler output */
void Assembler::outputAssembledFile(std::ofstream &outputFile)
{
    ASM::word_t addrCounter = 0;
    bytecode_t addrCounterFormatted;

    if (outputFile.is_open())
    {
        for (auto &section : Assembler::SectionTables)
        {
            addrCounterFormatted = Assembler::encodeLiteralToBytecode(addrCounter);
            outputFile << Assembler::parseBytecodeToAddress(addrCounterFormatted) << " <" << section.first << ">:" << std::endl;

            for (auto &row : section.second.row)
            {
                addrCounterFormatted = Assembler::encodeLiteralToBytecode(addrCounter);
                outputFile << Assembler::parseBytecodeToAddress(addrCounterFormatted) << ": " << row.bytecode << '\t' << '\t' << row.description << std::endl;
                addrCounter += (ASM::word_t)row.size;
            }

            outputFile << std::endl;
        }
    }
    else
    {
        Assembler::reportErrorAndExit(Assembler::ErrorCode::WRITE_FAILED);
    }
}

int main(int argc, char *argv[])
{
    Assembler::assertCorrectProgramCall(argc, argv);

    /* refrences to input resource files */
    std::string inputFilePath((argc == 2) ? argv[1] : argv[3]);
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

            Assembler::storeLine(Program, tokens, lineCounter, Assembler::LineTag::NONE);
            token = tokens[0];

            if (Assembler::isLabelDeclaredAt(tokens, 0))
            {
                token.erase(token.length() - 1, 1);

                Assembler::assertValidLabelDeclarationOn(lineCounter, token, currentSection);
                Assembler::insertLabel(token, currentSection, lineCounter);

                // jump to next line when the line only holds a label
                if (tokens.size() == 1)
                {
                    Assembler::patchLineTag(Program, Assembler::LineTag::LABEL_ONLY_TAG);
                    lineCounter = Assembler::prepareForNextLine(tokens, lineCounter);
                    continue;
                }
            }

            // reference tokens corretly based on whether a label is at the start of the line
            labelOffset = Assembler::setLabelOffsetFlag(tokens);
            token = tokens[labelOffset];

            if (Assembler::isDirectiveDeclaration(token))
            {
                ASM::Directive dir = Assembler::assertValidDirective(token, lineCounter);
                Assembler::patchLineTag(Program, (labelOffset) ? Assembler::LineTag::LABEL_DIR_TAG : Assembler::LineTag::DIR_TAG);

                if (dir == ASM::Directive::DOT_END)
                {
                    // skip to the second pass of assembling
                    while (std::getline(sourceProgram, programLine))
                    {
                        lineCounter++;
                    }
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
                    // only calculate the offset, the initialization in the section table is done in the second pass
                    Assembler::assertSkipDirectiveOperandDeclared(tokens, labelOffset, lineCounter);
                    literalBits = Assembler::tryParseLiteral(tokens[labelOffset + 1], lineCounter);
                    //Assembler::initBytesWithZero(currentSection, literalBits);
                    Assembler::updateSectionLocationCounter(currentSection, literalBits);
                    break;

                case ASM::Directive::DOT_EQU:
                {
                    Assembler::assertEquDirectiveCorrectOperandCount(tokens, labelOffset, lineCounter);
                    literalBits = Assembler::tryParseLiteral(tokens[labelOffset + 2], lineCounter);
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
                operandCount = Assembler::calcInstrOperandCountFromLine(tokens, labelOffset);
                Assembler::assertValidInstructionCall(instrID, currentSection, operandCount, lineCounter);

                Assembler::patchLineTag(Program, (labelOffset) ? Assembler::LineTag::LABEL_INSTR_TAG : Assembler::LineTag::INSTR_TAG);

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
    sourceProgram.close();
    Assembler::resetSectionLocationCounters();

    /* perform assembler second pass */
    try
    {
        bool endDirectiveDetected = false;
        int endDirectiveDeclaredOn = 0;
        currentSection = Assembler::Section::NO_SECTION;

        for (auto &line : Program)
        {
            switch (line.tag)
            {
            case Assembler::LineTag::LABEL_ONLY_TAG:
                // labels have already been processed in the first pass
                break;

            case Assembler::LineTag::DIR_TAG:
                Assembler::processDirective(line.tokens, 0, line.lineNumber, currentSection);
                endDirectiveDetected = Assembler::endDirectiveDetected(line.tokens, 0, line.lineNumber);
                break;

            case Assembler::LineTag::LABEL_DIR_TAG:
                Assembler::processDirective(line.tokens, 1, line.lineNumber, currentSection);
                endDirectiveDetected = Assembler::endDirectiveDetected(line.tokens, 1, line.lineNumber);
                break;

            case Assembler::LineTag::INSTR_TAG:
                Assembler::processInstruction(line.tokens, 0, line.lineNumber, currentSection);
                break;

            case Assembler::LineTag::LABEL_INSTR_TAG:
                Assembler::processInstruction(line.tokens, 1, line.lineNumber, currentSection);
                break;
            }

            if (endDirectiveDetected)
            {
                // finish processing on the .end directive
                endDirectiveDeclaredOn = line.lineNumber;
                break;
            }
        }

        Assembler::assertProgramHasEnd(endDirectiveDetected, lineCounter);
        if (endDirectiveDeclaredOn < lineCounter)
        {
            Assembler::reportWarning(endDirectiveDeclaredOn, Assembler::WarningCode::IGNORE_AFTER_END);
        }
    }
    catch (Assembler::ErrorCode code)
    {
        Assembler::reportErrorAndExit(code);
    }

    /* output assembled file */
    std::stringstream outPathDefault;
    outPathDefault << "." << (inputFilePath.substr(inputFilePath.find_last_of('/'), inputFilePath.find_last_of('.') - inputFilePath.find_last_of('/'))) << ".txt";
    std::string outputFilePath = (argc == 2)
                                     ? outPathDefault.str()
                                     : argv[2];
    std::ofstream assembledObjectFile(outputFilePath);

    std::cout << "Assembling output..." << std::endl;
    Assembler::outputAssembledFile(assembledObjectFile);
    std::cout << "Assembling finished. Open file " << outputFilePath << " for the text representation of the assembled object file." << std::endl;

    /* DEBUG */
    std::cout << std::endl
              << "<<<ASSEMBLER_DEBUG_DUMP>>>" << std::endl
              << std::endl;
    //DebugAssembler::debugTokenization(Program);
    DebugAssembler::debugSymbolTable();
    DebugAssembler::debugSectionTables();
    DebugAssembler::debugRelocationTable();

    return 0;
}