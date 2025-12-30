#include "../headers/semantic/semantic_analyzer.hpp"
FunctionDecl* SemanticAnalyzer::lookupFunction(const std::string& name) {
    // current module
    if (auto it = currentModule->publicSymbols.find(name);
        it != currentModule->publicSymbols.end()) {
        return dynamic_cast<FunctionDecl*>(it->second);
    }

    // imported modules
    for (auto& imp : currentModule->imports) {
        auto& pub = imp->resolvedModule->publicSymbols;
        if (auto it = pub.find(name); it != pub.end()) {
            return dynamic_cast<FunctionDecl*>(it->second);
        }
    }

    return nullptr;
}
FunctionDecl* SemanticAnalyzer::lookupMethod(
    StructDecl* st,
    const std::string& name
) {
    if (!st)
        return nullptr;

    for (auto& method : st->methods) {
        if (method->name == name)
            return method.get();
    }

    return nullptr;
}

bool isLValue(Expr* e) {
    return dynamic_cast<IdentifierExpr*>(e)
        || dynamic_cast<MemberAccessExpr*>(e)
        || (dynamic_cast<UnaryExpr*>(e) &&
            static_cast<UnaryExpr*>(e)->op == UnaryOp::Dereference);
}

bool isNumeric(const TypeInfo& t) {
    return t.name == "i16" || t.name == "i32" || t.name == "i64"
        || t.name == "f16" || t.name == "f32" || t.name == "f64";
}

void SemanticAnalyzer::pushScope() {
    scopes.emplace_back();
}

void SemanticAnalyzer::popScope() {
    scopes.pop_back();
}

void SemanticAnalyzer::declare(const std::string& name, Symbol sym) {
    auto& scope = scopes.back();
    if (scope.count(name)) {
        error("Redeclaration of symbol: " + name, sym.node->loc);
    }
    scope[name] = sym;
}

Symbol* SemanticAnalyzer::resolve(const std::string& name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return &found->second;
    }
    return nullptr;
}
void SemanticAnalyzer::analyze() {
    for (auto& mod : program->modules) {
        analyzeModule(mod.get());
    }
}
void SemanticAnalyzer::analyzeModule(Module* mod) {
    currentModule = mod;

    // Pass 1: register public symbols
    for (auto& decl : mod->declarations) {
        if (auto s = dynamic_cast<StructDecl*>(decl.get())) {
            mod->publicSymbols[s->name] = s;
        } else if (auto f = dynamic_cast<FunctionDecl*>(decl.get())) {
            mod->publicSymbols[f->name] = f;
        }
    }

    // Pass 2: analyze contents
    for (auto& decl : mod->declarations) {
        if (auto s = dynamic_cast<StructDecl*>(decl.get()))
            analyzeStruct(s);
        else if (auto f = dynamic_cast<FunctionDecl*>(decl.get()))
            analyzeFunction(f);
    }

    currentModule = nullptr;
}
StructDecl* SemanticAnalyzer::lookupStruct(const std::string& name) {
    // current module
    if (auto it = currentModule->publicSymbols.find(name);
        it != currentModule->publicSymbols.end())
        return dynamic_cast<StructDecl*>(it->second);

    // imported modules
    for (auto& imp : currentModule->imports) {
        auto& pub = imp->resolvedModule->publicSymbols;
        if (auto it = pub.find(name); it != pub.end())
            return dynamic_cast<StructDecl*>(it->second);
    }

    return nullptr;
}

void SemanticAnalyzer::analyzeStruct(StructDecl* s) {
    currentStruct = s;

    // validate field types
    for (auto& field : s->fields) {
        if (field.type.empty()) {
            error("Invalid field type", s->loc);
        }
    }

    // analyze methods
    for (auto& method : s->methods) {
        analyzeFunction(method.get());
    }

    currentStruct = nullptr;
}
void SemanticAnalyzer::analyzeFunction(FunctionDecl* f) {
    currentFunction = f;

    pushScope();

    // implicit `this`
    if (f->isMethod) {
        declare(
            "this",
            {
                SymbolKind::Variable,
                { f->structName, 1 },
                f
            }
        );
    }

    // parameters
    for (auto& p : f->params) {
        declare(
            p.name,
            {
                SymbolKind::Parameter,
                p.t ,
                f
            }
        );
    }

    analyzeStmt(f->body.get());

    popScope();
    currentFunction = nullptr;
}
bool isComparison(BinaryOp op) {
    switch (op) {
        case BinaryOp::Equal:
        case BinaryOp::NotEqual:
        case BinaryOp::Less:
        case BinaryOp::LessEqual:
        case BinaryOp::Greater:
        case BinaryOp::GreaterEqual:
            return true;
        default:
            return false;
    }
}

void SemanticAnalyzer::analyzeStmt(Stmt* stmt) {
    if (!stmt) return;

    // ---------- Block ----------
    if (auto block = dynamic_cast<BlockStmt*>(stmt)) {
        pushScope();
        for (auto& s : block->statements)
            analyzeStmt(s.get());
        popScope();
    }

    // ---------- Return ----------
    else if (auto ret = dynamic_cast<ReturnStmt*>(stmt)) {
        if (currentFunction->type.name == "void") {
            if (ret->value)
                error("Void function cannot return a value", ret->loc);
        } else {
            if (!ret->value)
                error("Missing return value", ret->loc);
            else {
                TypeInfo retType = analyzeExpr(ret->value.get());
                if (!(retType == currentFunction->type))
                    error("Return type mismatch", ret->loc);
            }
        }
    }

    // ---------- Expression Statement ----------
    else if (auto exprStmt = dynamic_cast<ExprStmt*>(stmt)) {
        analyzeExpr(exprStmt->expr.get());
    }

    // ---------- If / Elif / Else ----------
    else if (auto ifs = dynamic_cast<IfStmt*>(stmt)) {
        // Analyze the main 'if' condition
        TypeInfo condType = analyzeExpr(ifs->condition.get());
        if (condType.name != "bool")
            error("Condition must be boolean", ifs->condition->loc);

        // Analyze 'then' branch
        analyzeStmt(ifs->thenBranch.get());

        // Analyze 'elif' clauses
        for (auto& elif : ifs->elifClauses) {
            TypeInfo elifCondType = analyzeExpr(elif.condition.get());
            if (elifCondType.name != "bool")
                error("Elif condition must be boolean", elif.condition->loc);

            analyzeStmt(elif.body.get());
        }

        // Analyze optional 'else' branch
        if (ifs->elseBranch)
            analyzeStmt(ifs->elseBranch.get());
    }

    // ---------- While ----------
    else if (auto wh = dynamic_cast<WhileStmt*>(stmt)) {
        TypeInfo condType = analyzeExpr(wh->condition.get());
        if (condType.name != "bool")
            error("While condition must be boolean", wh->condition->loc);

        analyzeStmt(wh->body.get());
    }

    // ---------- For ----------
    else if (auto fr = dynamic_cast<ForStmt*>(stmt)) {
        pushScope();

        if (fr->initializer)
            analyzeStmt(fr->initializer.get());

        if (fr->condition) {
            TypeInfo condType = analyzeExpr(fr->condition.get());
            if (condType.name != "bool")
                error("For loop condition must be boolean", fr->condition->loc);
        }

        analyzeStmt(fr->body.get());

        if (fr->increment)
            analyzeExpr(fr->increment.get());

        popScope();
    }

    // ---------- Variable Declaration ----------
    else if (auto v = dynamic_cast<VarDeclStmt*>(stmt)) {
        TypeInfo t = TypeInfo{v->typeName,v->pointerDepth};
        if (v->initializer)
            t = analyzeExpr(v->initializer.get());

        declare(v->name, {SymbolKind::Variable, t, currentFunction});
    }else {
        error("Unknown statement type", stmt->loc);
    }
}
TypeInfo SemanticAnalyzer::analyzeExpr(Expr* expr) {

    // ---------- identifier ----------
    if (auto id = dynamic_cast<IdentifierExpr*>(expr)) {
        Symbol* sym = resolve(id->name);
        if (!sym) {
            error("Undefined identifier: " + id->name, id->loc);
            return {"error", 0};
        }
        expr->type = sym->type;
        return expr->type;
    }

    // ---------- this ----------
    if (dynamic_cast<ThisExpr*>(expr)) {
        if (!currentStruct) {
            error("'this' outside method", expr->loc);
            return {"error", 0};
        }
        expr->type = { currentStruct->name, 1 };
        return expr->type;
    }

    // ---------- binary ----------
    if (auto bin = dynamic_cast<BinaryExpr*>(expr)) {
        TypeInfo lhs = analyzeExpr(bin->lhs.get());
        TypeInfo rhs = analyzeExpr(bin->rhs.get());

        if (lhs.empty() || rhs.empty())
            return {"error", 0};

        // assignment
        if (bin->op == BinaryOp::Assign) {
            if (!isLValue(bin->lhs.get())) {
                error("Left-hand side is not assignable", bin->loc);
                return {"error", 0};
            }

            if (!(lhs == rhs)) {
                error("Assignment type mismatch", bin->loc);
            }

            expr->type = lhs;
            return expr->type;
        }

        // comparisons
        if (isComparison(bin->op)) {
            if (!(lhs == rhs)) {
                error("Comparison type mismatch", bin->loc);
            }
            expr->type = {"bool", 0};
            return expr->type;
        }

        // logical operators
        if (bin->op == BinaryOp::And || bin->op == BinaryOp::Or) {
            if (lhs.name != "bool" || rhs.name != "bool") {
                error("Logical operators require bool operands", bin->loc);
            }
            expr->type = {"bool", 0};
            return expr->type;
        }

        // arithmetic
        if (!isNumeric(lhs) || !(lhs == rhs)) {
            error("Invalid arithmetic operands", bin->loc);
            return {"error", 0};
        }

        expr->type = lhs;
        return expr->type;
    }

    // ---------- unary ----------
    if (auto un = dynamic_cast<UnaryExpr*>(expr)) {
        TypeInfo t = analyzeExpr(un->operand.get());

        if (t.empty())
            return {"error", 0};

        switch (un->op) {
            case UnaryOp::AddressOf:
                if (!isLValue(un->operand.get())) {
                    error("Cannot take address of rvalue", expr->loc);
                }
                t.pointerDepth++;
                break;

            case UnaryOp::Dereference:
                if (t.pointerDepth == 0) {
                    error("Cannot dereference non-pointer", expr->loc);
                } else {
                    t.pointerDepth--;
                }
                break;

            case UnaryOp::Not:
                if (t.name != "bool") {
                    error("Logical not requires bool operand", expr->loc);
                }
                t = {"bool", 0};
                break;

            case UnaryOp::Negative:
                if (!isNumeric(t)) {
                    error("Unary minus requires numeric operand", expr->loc);
                }
                break;
        }

        expr->type = t;
        return expr->type;
    }

    // ---------- member access ----------
    if (auto mem = dynamic_cast<MemberAccessExpr*>(expr)) {
        TypeInfo base = analyzeExpr(mem->object.get());

        if (base.empty())
            return {"error", 0};

        // implicit dereference
        if (base.pointerDepth > 0)
            base.pointerDepth--;

        StructDecl* st = lookupStruct(base.name);
        if (!st) {
            error("Unknown struct type: " + base.name, mem->loc);
            return {"error", 0};
        }

        for (auto& field : st->fields) {
            if (field.name == mem->member) {
                expr->type = field.type;
                return expr->type;
            }
        }

        error("Unknown member: " + mem->member, mem->loc);
        return {"error", 0};
    }

    // ---------- call ----------
    if (auto call = dynamic_cast<CallExpr*>(expr)) {

        // function call: foo()
        if (auto id = dynamic_cast<IdentifierExpr*>(call->callee.get())) {
            FunctionDecl* fn = lookupFunction(id->name);
            if (!fn) {
                error("Unknown function: " + id->name, id->loc);
                return {"error", 0};
            }

            if (call->arguments.size() != fn->params.size()) {
                error("Argument count mismatch", call->loc);
            }

            for (size_t i = 0; i < call->arguments.size(); i++) {
                TypeInfo arg = analyzeExpr(call->arguments[i].get());
                if (!(arg == fn->params[i].t)) {
                    error("Argument type mismatch", call->arguments[i]->loc);
                }
            }

            expr->type = fn->type;
            return expr->type;
        }

        // method call: obj.method()
        if (auto mem = dynamic_cast<MemberAccessExpr*>(call->callee.get())) {
            TypeInfo base = analyzeExpr(mem->object.get());

            if (base.pointerDepth > 0)
                base.pointerDepth--;

            StructDecl* st = lookupStruct(base.name);
            if (!st) {
                error("Unknown struct type: " + base.name, mem->loc);
                return {"error", 0};
            }

            FunctionDecl* method = lookupMethod(st, mem->member);
            if (!method) {
                error("Unknown method: " + mem->member, mem->loc);
                return {"error", 0};
            }

            if (call->arguments.size() != method->params.size()) {
                error("Argument count mismatch", call->loc);
            }

            for (size_t i = 0; i < call->arguments.size(); i++) {
                TypeInfo arg = analyzeExpr(call->arguments[i].get());
                if (!(arg == method->params[i].t)) {
                    error("Argument type mismatch", call->arguments[i]->loc);
                }
            }

            expr->type = method->type;
            return expr->type;
        }

        error("Invalid call expression", expr->loc);
        return {"error", 0};
    }

    // ---------- fallback ----------
    error("Invalid expression", expr->loc);
    return {"error", 0};
}