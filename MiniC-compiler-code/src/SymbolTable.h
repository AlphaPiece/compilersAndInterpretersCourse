//
// Created by Fan Long on 2020/12/5.
//

#ifndef MINICC_SYMBOLTABLE_H
#define MINICC_SYMBOLTABLE_H

//add more header files if your want
//You may need assert function
#include <cassert>
#include "Types.h"
#include <map>

namespace llvm {
    class Value;
}

namespace minicc {


    struct VarSymbolEntry {
        Type VarType;
        llvm::Value *LLVMValue;

        explicit VarSymbolEntry(Type varType) : VarType(varType), LLVMValue(nullptr) { }
    };

    class VarSymbolTable {

        std::map<std::string, VarSymbolEntry> Table;

    public:
        //define your member variables and functions
        // mymap.insert ( std::pair<char,int>('a',100) );

        void insertSymbol(std::pair<std::string, VarSymbolEntry> pair) {
            Table.insert(pair);
        }

        VarSymbolEntry *findSymbol(std::string key) {
            std::map<std::string, VarSymbolEntry>::iterator it = Table.find(key);
            if (it != Table.end()) {
                return &(it->second);
            }
            return nullptr;
        }

        bool containsSymbol(std::string key) {
            if (findSymbol(key) == nullptr) {
                return false;
            }
            return true;
        }

        void setLLVMValue(const std::string &name, llvm::Value *val) {
            auto it = Table.find(name);
            assert(it != Table.end());
            it->second.LLVMValue = val;
        }
    };

    struct FuncSymbolEntry {
        Type ReturnType;
        std::vector<Type> ParameterTypes;
        bool HasBody;

        FuncSymbolEntry(Type retType, const std::vector<Type> &paraTypes, bool hasBody) : ReturnType(retType), ParameterTypes(paraTypes), HasBody(hasBody) { }
    };

    class FuncSymbolTable {
        std::map<std::string, FuncSymbolEntry> Table;
    public:
        //define your member variables and functions
    
        void insertSymbol(std::pair<std::string, FuncSymbolEntry> pair) { 
            Table.insert(pair); 
        }

        FuncSymbolEntry *findSymbol(std::string key) {
            std::map<std::string, FuncSymbolEntry>::iterator it = Table.find(key);
            if (it != Table.end()) {
                return &(it->second);
            }
            return nullptr;
        }

        bool containsSymbol(std::string key) {
            if (findSymbol(key) == nullptr) {
                return false;
            }
            return true;
        }

        std::map<std::string, FuncSymbolEntry>::iterator begin() {
            return Table.begin();
        }

        std::map<std::string, FuncSymbolEntry>::iterator end() {
            return Table.end();
        }
    };
}

#endif //MINICC_SYMBOLTABLE_H
