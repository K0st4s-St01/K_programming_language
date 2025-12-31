#include "../headers/codegen/codegen.hpp"
#include <iostream>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

llvm::Value* CodeGen::createAlloca(
    llvm::Function* fn,
    llvm::Type* type,
    const std::string& name
) {
    llvm::IRBuilder<> tmp(
        &fn->getEntryBlock(),
        fn->getEntryBlock().begin()
    );
    return tmp.CreateAlloca(type, nullptr, name);
}
void CodeGen::pushScope() {
    scopes.emplace_back();
}

void CodeGen::popScope() {
    scopes.pop_back();
}


CodeGen::CodeGen(Program* program) {
    this->program = program;
    this->module=std::make_unique<llvm::Module>("main",context);
    this->builder=std::make_unique<llvm::IRBuilder<>>(context);
}
void CodeGen::generate() {
    for (auto& mod : program->modules) {
    // Pass 1: declare structs (opaque)
    for (auto& decl : mod->declarations)
        if (auto s = dynamic_cast<StructDecl*>(decl.get()))
            declareStruct(s);

    // Pass 2: define struct bodies
    for (auto& decl : mod->declarations)
        if (auto s = dynamic_cast<StructDecl*>(decl.get()))
            defineStruct(s);

    // Pass 3: functions
    for (auto& decl : mod->declarations)
        if (auto f = dynamic_cast<FunctionDecl*>(decl.get()))
            generateFunction(f);
    }
}
void CodeGen::declareStruct(StructDecl* s) {
    structTypes[s->name] =
        llvm::StructType::create(context, s->name);

    unsigned idx = 0;
    for (auto& field : s->fields)
        structFieldIndex[s->name][field.name] = idx++;
}
void CodeGen::defineStruct(StructDecl* s) {
    llvm::StructType* st = structTypes[s->name];

    std::vector<llvm::Type*> fields;
    for (auto& field : s->fields)
        fields.push_back(getLLVMType(field.type));

    st->setBody(fields, false);
}

llvm::Function* CodeGen::generateFunction(FunctionDecl* f) {
    
    std::vector<llvm::Type*> paramTypes;
    for (auto& p : f->params)
        paramTypes.push_back(getLLVMType(p.t));

    llvm::FunctionType* funcType =
        llvm::FunctionType::get(
            getLLVMType(f->type),
            paramTypes,
            false
        );

    llvm::Function* function =
        llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            f->name,
            module.get()
        );

    currentFunction = function;

    llvm::BasicBlock* entry =
        llvm::BasicBlock::Create(context, "entry", function);
    builder->SetInsertPoint(entry);

    // 🔑 New scope
    pushScope();

    // ✅ Allocate + store parameters
    unsigned idx = 0;
    for (auto& arg : function->args()) {
        arg.setName(f->params[idx].name);

        llvm::Value* alloca =
            createAlloca(
                function,
                arg.getType(),
                arg.getName().str()
            );

        builder->CreateStore(&arg, alloca);
        symbolTable[arg.getName().str()] = alloca;

        idx++;
    }

    generateStmt(f->body.get());

    if (f->type.name == "void" &&
        !entry->getTerminator()) {
        builder->CreateRetVoid();
    }

    popScope();
    llvm::verifyFunction(*function);
    return function;
}
llvm::Value* CodeGen::generateStmt(Stmt* stmt) {
    if (auto f = dynamic_cast<ForStmt*>(stmt))
        return generateFor(f);

    if (auto w = dynamic_cast<WhileStmt*>(stmt))
        return generateWhile(w);

    if (auto block = dynamic_cast<BlockStmt*>(stmt)) {
        pushScope();
        llvm::Value* last = nullptr;
        for (auto& s : block->statements)
            last = generateStmt(s.get());
        popScope();
        return last;
    }
    if (dynamic_cast<BreakStmt*>(stmt)) {
    if (loopStack.empty()) {
        error("'break' outside loop", stmt->loc);
        return nullptr;
    }
    builder->CreateBr(loopStack.back().breakBB);
    return nullptr;
}

if (dynamic_cast<ContinueStmt*>(stmt)) {
    if (loopStack.empty()) {
        error("'continue' outside loop", stmt->loc);
        return nullptr;
    }
    builder->CreateBr(loopStack.back().continueBB);
    return nullptr;
}

    if (auto v = dynamic_cast<VarDeclStmt*>(stmt)) {
    llvm::Type* ty = getLLVMType({v->typeName, v->pointerDepth});

    llvm::Value* alloca =
        createAlloca(currentFunction, ty, v->name);

    symbolTable[v->name] = alloca;

    if (v->initializer) {
        llvm::Value* init = generateExpr(v->initializer.get());
        builder->CreateStore(init, alloca);
    }

    return alloca;
    }

    if (auto ret = dynamic_cast<ReturnStmt*>(stmt)) {
        llvm::Value* val = nullptr;
        if (ret->value)
            val = generateExpr(ret->value.get());
        if (val)
            return builder->CreateRet(val);
        else
            return builder->CreateRetVoid();
    }

    if (auto exprStmt = dynamic_cast<ExprStmt*>(stmt)) {
        return generateExpr(exprStmt->expr.get());
    }

    if (auto ifs = dynamic_cast<IfStmt*>(stmt)) {
        llvm::Value* cond = generateExpr(ifs->condition.get());
        cond = builder->CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0));

        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then", currentFunction);
        llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context, "else", currentFunction);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "ifcont", currentFunction);

        builder->CreateCondBr(cond, thenBB, elseBB);

        builder->SetInsertPoint(thenBB);
        generateStmt(ifs->thenBranch.get());
        builder->CreateBr(mergeBB);

        builder->SetInsertPoint(elseBB);
        if (ifs->elseBranch)
            generateStmt(ifs->elseBranch.get());
        builder->CreateBr(mergeBB);

        builder->SetInsertPoint(mergeBB);
        return nullptr;
    }

    return nullptr; // for unhandled statements
}
llvm::Value* CodeGen::generateExpr(Expr* expr) {
    // ---------- integer literal ----------
    if (auto lit = dynamic_cast<IntLiteralExpr*>(expr)) {
        return llvm::ConstantInt::get(
            getLLVMType(lit->type),
            lit->value
        );
    }

    // ---------- identifier (LOAD) ----------
    if (auto id = dynamic_cast<IdentifierExpr*>(expr)) {
        auto it = symbolTable.find(id->name);
        if (it == symbolTable.end())
            return nullptr;

        llvm::Value* ptr = it->second;
        return builder->CreateLoad(
            getLLVMType(expr->type),
            ptr,
            id->name + "_val"
        );
    }

    // ---------- binary ----------
    if (auto bin = dynamic_cast<BinaryExpr*>(expr)) {
        return generateBinary(bin);   // 🔥 FIX
    }

    // ---------- unary ----------
    if (auto un = dynamic_cast<UnaryExpr*>(expr)) {
        return generateUnary(un);
    }

    // ---------- call ----------
    if (auto call = dynamic_cast<CallExpr*>(expr)) {
        return generateCall(call);
    }

    // ---------- member access (rvalue load) ----------
    if (auto mem = dynamic_cast<MemberAccessExpr*>(expr)) {
        llvm::Value* ptr = generateMemberPtr(mem);
        return builder->CreateLoad(
            getLLVMType(expr->type),
            ptr,
            mem->member + "_val"
        );
    }

    return nullptr;
}

llvm::Value* CodeGen::generateBinary(BinaryExpr* bin) {
    llvm::Value* lhs = generateExpr(bin->lhs.get());
    llvm::Value* rhs = generateExpr(bin->rhs.get());

    if (!lhs || !rhs) return nullptr;

    switch (bin->op) {
        // Arithmetic
        case BinaryOp::Add:
            return builder->CreateAdd(lhs, rhs, "addtmp");
        case BinaryOp::Sub:
            return builder->CreateSub(lhs, rhs, "subtmp");
        case BinaryOp::Mul:
            return builder->CreateMul(lhs, rhs, "multmp");
        case BinaryOp::Div:
            // signed division; use CreateUDiv for unsigned
            return builder->CreateSDiv(lhs, rhs, "divtmp");

        // Comparison (signed integers)
        case BinaryOp::Equal:
            return builder->CreateICmpEQ(lhs, rhs, "eqtmp");
        case BinaryOp::NotEqual:
            return builder->CreateICmpNE(lhs, rhs, "netmp");
        case BinaryOp::Less:
            return builder->CreateICmpSLT(lhs, rhs, "lttmp");
        case BinaryOp::LessEqual:
            return builder->CreateICmpSLE(lhs, rhs, "letmp");
        case BinaryOp::Greater:
            return builder->CreateICmpSGT(lhs, rhs, "gttmp");
        case BinaryOp::GreaterEqual:
            return builder->CreateICmpSGE(lhs, rhs, "getmp");

        // Logical (booleans)
        case BinaryOp::And:
            return generateLogicalAnd(bin);
        case BinaryOp::Or:
            return generateLogicalOr(bin);
        case BinaryOp::Xor:
            return builder->CreateXor(lhs, rhs, "xortmp");

        // Assignment handled elsewhere (store to pointer)
        case BinaryOp::Assign:
            if (auto lval = dynamic_cast<IdentifierExpr*>(bin->lhs.get())) {
                llvm::Value* ptr = symbolTable[lval->name];
                return builder->CreateStore(rhs, ptr);
            } else if (auto mem = dynamic_cast<MemberAccessExpr*>(bin->lhs.get())) {
                llvm::Value* ptr = generateMemberPtr(mem);
                return builder->CreateStore(rhs, ptr);
            }
            return nullptr;

        default:
            return nullptr;
    }
}
llvm::Value* CodeGen::generateUnary(UnaryExpr* un) {
    llvm::Value* val = generateExpr(un->operand.get());

    switch (un->op) {
        case UnaryOp::Negative:
            return builder->CreateNeg(val, "negtmp");

        case UnaryOp::Not:
            return builder->CreateICmpEQ(
                val,
                llvm::ConstantInt::get(val->getType(), 0),
                "nottmp"
            );

        case UnaryOp::AddressOf:
            // operand must already be an lvalue
            if (auto id = dynamic_cast<IdentifierExpr*>(un->operand.get()))
                return symbolTable[id->name];
            break;

        case UnaryOp::Dereference:
            return builder->CreateLoad(
                getLLVMType(un->type),
                val,
                "deref"
            );
    }

    return nullptr;
}

void CodeGen::error(const std::string& message,SourceLocation loc) {
    std::cerr << "[Syntax Error] "
              << loc.to_string()
              << ": "
              << message
              << "\n";

}

llvm::Value* CodeGen::generateMemberPtr(MemberAccessExpr* mem) {
    // 1️⃣ Generate base expression
    llvm::Value* base = generateExpr(mem->object.get());
    if (!base)
        return nullptr;

    TypeInfo baseType = mem->object->type;

    // 2️⃣ Must be struct or pointer-to-struct
    if (baseType.pointerDepth > 1) {
        error("Too many pointer indirections for member access", mem->loc);
        return nullptr;
    }

    // 3️⃣ Lookup LLVM struct type
    auto it = structTypes.find(baseType.name);
    if (it == structTypes.end()) {
        error("Unknown struct type: " + baseType.name, mem->loc);
        return nullptr;
    }

    llvm::StructType* structTy = it->second;

    if (structTy->isOpaque()) {
        error("Struct is opaque (not defined yet): " + baseType.name, mem->loc);
        return nullptr;
    }

    // 4️⃣ Get field index
    auto& fieldMap = structFieldIndex[baseType.name];
    auto fit = fieldMap.find(mem->member);
    if (fit == fieldMap.end()) {
        error("Unknown struct member: " + mem->member, mem->loc);
        return nullptr;
    }

    unsigned fieldIndex = fit->second;

    // 5️⃣ Ensure base is a pointer
    llvm::Value* basePtr = nullptr;

    if (baseType.pointerDepth == 0) {
        // value → allocate temporary
        basePtr = builder->CreateAlloca(structTy, nullptr, "tmp_struct");
        builder->CreateStore(base, basePtr);
    } else {
        // already pointer
        basePtr = base;
    }

    // 6️⃣ Create GEP (modern API)
    return builder->CreateStructGEP(
        structTy,
        basePtr,
        fieldIndex,
        mem->member + "_ptr"
    );
}
llvm::Type* CodeGen::getLLVMType(TypeInfo t) {
    llvm::Type* base = nullptr;

    // ---------- primitives ----------
    if (t.name == "void") {
        base = llvm::Type::getVoidTy(context);
    }
    else if (t.name == "bool") {
        base = llvm::Type::getInt1Ty(context);
    }
    else if (t.name == "i16") {
        base = llvm::Type::getInt16Ty(context);
    }
    else if (t.name == "i32") {
        base = llvm::Type::getInt32Ty(context);
    }
    else if (t.name == "i64") {
        base = llvm::Type::getInt64Ty(context);
    }else if (t.name == "f32") {
        base = llvm::Type::getFloatTy(context);
    }
    else if (t.name == "f64") {
        base = llvm::Type::getDoubleTy(context);
    }

    // ---------- struct ----------
    else {
        auto it = structTypes.find(t.name);
        if (it == structTypes.end()) {
            error("Unknown type: " + t.name, {});
            return llvm::Type::getVoidTy(context);
        }
        base = it->second;
    }

    // ---------- pointers ----------
    for (int i = 0; i < t.pointerDepth; i++) {
        base = llvm::PointerType::getUnqual(context);
    }

    return base;
}
llvm::Value* CodeGen::generateCall(CallExpr* call) {

    // METHOD CALL
    if (auto mem = dynamic_cast<MemberAccessExpr*>(call->callee.get())) {

        llvm::Value* thisPtr = generateExpr(mem->object.get());

        std::string mangled =
            mem->object->type.name + "_" + mem->member;

        llvm::Function* callee =
            module->getFunction(mangled);

        if (!callee) {
            error("Unknown method: " + mem->member, call->loc);
            return nullptr;
        }

        std::vector<llvm::Value*> args;
        args.push_back(thisPtr); // 🔑 implicit this

        for (auto& a : call->arguments)
            args.push_back(generateExpr(a.get()));

        return builder->CreateCall(callee, args);
    }

    // NORMAL FUNCTION
    auto id = dynamic_cast<IdentifierExpr*>(call->callee.get());
    if (!id) {
        error("Call target is not callable", call->loc);
        return nullptr;
    }

    llvm::Function* callee = module->getFunction(id->name);
    if (!callee) {
        error("Unknown function: " + id->name, call->loc);
        return nullptr;
    }

    std::vector<llvm::Value*> args;
    for (auto& arg : call->arguments)
        args.push_back(generateExpr(arg.get()));

    return builder->CreateCall(callee, args);
}

llvm::Value* CodeGen::generateWhile(WhileStmt* w) {
    llvm::Function* fn = currentFunction;

    llvm::BasicBlock* condBB =
        llvm::BasicBlock::Create(context, "while.cond", fn);
    llvm::BasicBlock* bodyBB =
        llvm::BasicBlock::Create(context, "while.body", fn);
    llvm::BasicBlock* endBB =
        llvm::BasicBlock::Create(context, "while.end", fn);

    // initial jump
    builder->CreateBr(condBB);

    // condition
    builder->SetInsertPoint(condBB);
    llvm::Value* cond = generateExpr(w->condition.get());
    cond = builder->CreateICmpNE(
        cond,
        llvm::ConstantInt::get(cond->getType(), 0)
    );
    builder->CreateCondBr(cond, bodyBB, endBB);

    // push loop context
    loopStack.push_back({
        endBB,   // break
        condBB  // continue
    });

    // body
    builder->SetInsertPoint(bodyBB);
    pushScope();
    generateStmt(w->body.get());
    popScope();

    if (!builder->GetInsertBlock()->getTerminator())
        builder->CreateBr(condBB);

    loopStack.pop_back();

    builder->SetInsertPoint(endBB);
    return nullptr;
}

llvm::Value* CodeGen::generateFor(ForStmt* f) {
    llvm::Function* fn = currentFunction;

    pushScope();

    if (f->initializer)
        generateStmt(f->initializer.get());

    llvm::BasicBlock* condBB =
        llvm::BasicBlock::Create(context, "for.cond", fn);
    llvm::BasicBlock* bodyBB =
        llvm::BasicBlock::Create(context, "for.body", fn);
    llvm::BasicBlock* incBB =
        llvm::BasicBlock::Create(context, "for.inc", fn);
    llvm::BasicBlock* endBB =
        llvm::BasicBlock::Create(context, "for.end", fn);

    builder->CreateBr(condBB);

    // condition
    builder->SetInsertPoint(condBB);
    llvm::Value* cond = generateExpr(f->condition.get());
    cond = builder->CreateICmpNE(
        cond,
        llvm::ConstantInt::get(cond->getType(), 0)
    );
    builder->CreateCondBr(cond, bodyBB, endBB);

    // push loop context
    loopStack.push_back({
        endBB, // break
        incBB  // continue
    });

    // body
    builder->SetInsertPoint(bodyBB);
    generateStmt(f->body.get());
    if (!builder->GetInsertBlock()->getTerminator())
        builder->CreateBr(incBB);

    // increment
    builder->SetInsertPoint(incBB);
    generateExpr(f->increment.get());
    builder->CreateBr(condBB);

    loopStack.pop_back();
    popScope();

    builder->SetInsertPoint(endBB);
    return nullptr;
}
llvm::Value* CodeGen::generateLogicalAnd(BinaryExpr* bin) {
    llvm::Function* fn = currentFunction;

    llvm::BasicBlock* lhsTrueBB =
        llvm::BasicBlock::Create(context, "and.lhs.true", fn);
    llvm::BasicBlock* mergeBB =
        llvm::BasicBlock::Create(context, "and.merge", fn);

    llvm::BasicBlock* lhsBB = builder->GetInsertBlock();

    llvm::Value* lhs = generateExpr(bin->lhs.get());
    lhs = builder->CreateICmpNE(
        lhs,
        llvm::ConstantInt::get(lhs->getType(), 0)
    );

    builder->CreateCondBr(lhs, lhsTrueBB, mergeBB);

    // RHS
    builder->SetInsertPoint(lhsTrueBB);
    llvm::Value* rhs = generateExpr(bin->rhs.get());
    rhs = builder->CreateICmpNE(
        rhs,
        llvm::ConstantInt::get(rhs->getType(), 0)
    );
    builder->CreateBr(mergeBB);

    // Merge
    builder->SetInsertPoint(mergeBB);
    llvm::PHINode* phi =
        builder->CreatePHI(llvm::Type::getInt1Ty(context), 2);

    phi->addIncoming(
        llvm::ConstantInt::getFalse(context),
        lhsBB
    );
    phi->addIncoming(rhs, lhsTrueBB);

    return phi;
}
llvm::Value* CodeGen::generateLogicalOr(BinaryExpr* bin) {
    llvm::Function* fn = currentFunction;

    llvm::BasicBlock* lhsFalseBB =
        llvm::BasicBlock::Create(context, "or.lhs.false", fn);
    llvm::BasicBlock* mergeBB =
        llvm::BasicBlock::Create(context, "or.merge", fn);

    llvm::BasicBlock* lhsBB = builder->GetInsertBlock();

    llvm::Value* lhs = generateExpr(bin->lhs.get());
    lhs = builder->CreateICmpNE(
        lhs,
        llvm::ConstantInt::get(lhs->getType(), 0)
    );

    builder->CreateCondBr(lhs, mergeBB, lhsFalseBB);

    // RHS
    builder->SetInsertPoint(lhsFalseBB);
    llvm::Value* rhs = generateExpr(bin->rhs.get());
    rhs = builder->CreateICmpNE(
        rhs,
        llvm::ConstantInt::get(rhs->getType(), 0)
    );
    builder->CreateBr(mergeBB);

    // Merge
    builder->SetInsertPoint(mergeBB);
    llvm::PHINode* phi =
        builder->CreatePHI(llvm::Type::getInt1Ty(context), 2);

    phi->addIncoming(
        llvm::ConstantInt::getTrue(context),
        lhsBB
    );
    phi->addIncoming(rhs, lhsFalseBB);

    return phi;
}
