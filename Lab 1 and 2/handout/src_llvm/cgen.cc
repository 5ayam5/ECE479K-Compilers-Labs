/*********************************************************************
 Intermediate code generator for COOL: SKELETON

 Read the comments carefully and add code to build an LLVM program
*********************************************************************/

#define EXTERN
#include "cgen.h"
#include <sstream>
#include <string>
#include <llvm/Support/FileSystem.h>

extern int cgen_debug, curr_lineno;

std::string concat_method_name(CgenNode *cls, Symbol name)
{
  return cls->get_type_name() + '_' + name->get_string();
}

/*********************************************************************
 For convenience, a large number of symbols are predefined here.
 These symbols include the primitive type and method names, as well
 as fixed names used by the runtime system. Feel free to add your
 own definitions as you see fit.
*********************************************************************/
EXTERN Symbol
    // required classes
    Object,
    IO, String, Int, Bool, Main,

    // class methods
    cool_abort, type_name, cool_copy, out_string, out_int, in_string, in_int,
    length, concat, substr,

    // class members
    val,

    // special symbols
    No_class,  // symbol that can't be the name of any user-defined class
    No_type,   // If e : No_type, then no code is generated for e.
    SELF_TYPE, // Special code is generated for new SELF_TYPE.
    self,      // self generates code differently than other references

    // extras
    arg, arg2, newobj, Mainmain, prim_string, prim_int, prim_bool;

// Initializing the predefined symbols.
static void initialize_constants(void)
{
  Object = idtable.add_string("Object");
  IO = idtable.add_string("IO");
  String = idtable.add_string("String");
  Int = idtable.add_string("Int");
  Bool = idtable.add_string("Bool");
  Main = idtable.add_string("Main");

  cool_abort = idtable.add_string("abort");
  type_name = idtable.add_string("type_name");
  cool_copy = idtable.add_string("copy");
  out_string = idtable.add_string("out_string");
  out_int = idtable.add_string("out_int");
  in_string = idtable.add_string("in_string");
  in_int = idtable.add_string("in_int");
  length = idtable.add_string("length");
  ::concat = idtable.add_string("concat");
  substr = idtable.add_string("substr");

  val = idtable.add_string("val");

  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  self = idtable.add_string("self");

  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  newobj = idtable.add_string("_newobj");
  Mainmain = idtable.add_string("main");
  prim_string = idtable.add_string("sbyte*");
  prim_int = idtable.add_string("int");
  prim_bool = idtable.add_string("bool");
}

/*********************************************************************

  CgenClassTable methods

*********************************************************************/

// CgenClassTable constructor orchestrates all code generation
CgenClassTable::CgenClassTable(Classes classes)
    : nds(), current_tag(0), context(), builder(this->context),
      the_module("module", this->context)
{
  if (cgen_debug)
    llvm::errs() << "Building CgenClassTable\n";
  // Make sure we have a scope, both for classes and for constants
  enterscope();

  // Create an inheritance tree with one CgenNode per class.
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  // First pass
  setup();

  // Second pass
  code_module();
  // Done with code generation: exit scopes
  exitscope();
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
  // The tree package uses these globals to annotate the classes built below.
  curr_lineno = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list. Thus, these classes exist, but are not part of the
  // inheritance hierarchy.

  // No_class serves as the parent of Object and the other special classes.
  Class_ noclasscls = class_(No_class, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
  delete noclasscls;

#ifdef LAB2
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  Class_ selftypecls = class_(SELF_TYPE, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
  delete selftypecls;
  //
  // Primitive types masquerading as classes. This is done so we can
  // get the necessary Symbols for the innards of String, Int, and Bool
  //
  Class_ primstringcls =
      class_(prim_string, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
  delete primstringcls;
#endif
  Class_ primintcls = class_(prim_int, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
  delete primintcls;
  Class_ primboolcls = class_(prim_bool, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
  delete primboolcls;
  //
  // The Object class has no parent class. Its methods are
  //    cool_abort() : Object    aborts the program
  //    type_name() : Str        returns a string representation of class name
  //    copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  Class_ objcls = class_(
      Object, No_class,
      append_Features(
          append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                 Object, no_expr())),
                          single_Features(method(type_name, nil_Formals(),
                                                 String, no_expr()))),
          single_Features(
              method(cool_copy, nil_Formals(), SELF_TYPE, no_expr()))),
      filename);
  install_class(new CgenNode(objcls, CgenNode::Basic, this));
  delete objcls;

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ intcls = class_(
      Int, Object, single_Features(attr(val, prim_int, no_expr())), filename);
  install_class(new CgenNode(intcls, CgenNode::Basic, this));
  delete intcls;

  //
  // Bool also has only the "val" slot.
  //
  Class_ boolcls = class_(
      Bool, Object, single_Features(attr(val, prim_bool, no_expr())), filename);
  install_class(new CgenNode(boolcls, CgenNode::Basic, this));
  delete boolcls;

#ifdef LAB2
  //
  // The class String has a number of slots and operations:
  //       val                                  the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  Class_ stringcls =
      class_(String, Object,
             append_Features(
                 append_Features(
                     append_Features(
                         single_Features(attr(val, prim_string, no_expr())),
                         single_Features(
                             method(length, nil_Formals(), Int, no_expr()))),
                     single_Features(method(::concat,
                                            single_Formals(formal(arg, String)),
                                            String, no_expr()))),
                 single_Features(
                     method(substr,
                            append_Formals(single_Formals(formal(arg, Int)),
                                           single_Formals(formal(arg2, Int))),
                            String, no_expr()))),
             filename);
  install_class(new CgenNode(stringcls, CgenNode::Basic, this));
  delete stringcls;
#endif

#ifdef LAB2
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  Class_ iocls = class_(
      IO, Object,
      append_Features(
          append_Features(
              append_Features(
                  single_Features(method(out_string,
                                         single_Formals(formal(arg, String)),
                                         SELF_TYPE, no_expr())),
                  single_Features(method(out_int,
                                         single_Formals(formal(arg, Int)),
                                         SELF_TYPE, no_expr()))),
              single_Features(
                  method(in_string, nil_Formals(), String, no_expr()))),
          single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
      filename);
  install_class(new CgenNode(iocls, CgenNode::Basic, this));
  delete iocls;
#endif
}

// install_classes enters a list of classes in the symbol table.
void CgenClassTable::install_classes(Classes cs)
{
  for (auto cls : cs)
  {
    install_class(new CgenNode(cls, CgenNode::NotBasic, this));
  }
}

// Add this CgenNode to the class list and the lookup table
void CgenClassTable::install_class(CgenNode *nd)
{
  Symbol name = nd->get_name();
  if (!this->find(name))
  {
    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds.push_back(nd);
    this->insert(name, nd);
  }
}

// Add this CgenNode to the special class list and the lookup table
void CgenClassTable::install_special_class(CgenNode *nd)
{
  Symbol name = nd->get_name();
  if (!this->find(name))
  {
    // The class name is legal, so add it to the list of special classes
    // and the symbol table.
    special_nds.push_back(nd);
    this->insert(name, nd);
  }
}

// CgenClassTable::build_inheritance_tree
void CgenClassTable::build_inheritance_tree()
{
  for (auto node : nds)
    set_relations(node);
}

// CgenClassTable::set_relations
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table. Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd)
{
  Symbol parent = nd->get_parent();
  auto parent_node = this->find(parent);
  if (!parent_node)
  {
    throw std::runtime_error("Class " + nd->get_name()->get_string() +
                             " inherits from an undefined class " +
                             parent->get_string());
  }
  nd->set_parent(parent_node);
}

// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for Lab1
void CgenClassTable::setup_external_functions()
{
  llvm::Type *i32 = llvm::Type::getInt32Ty(this->context),
             *i8_ptr = llvm::Type::getInt8PtrTy(this->context),
             *void_ = llvm::Type::getVoidTy(this->context);
  // setup function: external int strcmp(sbyte*, sbyte*)
  create_llvm_function("strcmp", i32, {i8_ptr, i8_ptr}, false);
  // setup function: external int printf(sbyte*, ...)
  create_llvm_function("printf", i32, {i8_ptr}, true);
  // setup function: external void abort(void)
  create_llvm_function("abort", void_, {}, false);
  // setup function: external i8* malloc(i32)
  create_llvm_function("malloc", i8_ptr, {i32}, false);

#ifdef LAB2
  // TODO: add code here
#endif
}

void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
  c->setup(current_tag++, depth);
  for (auto child : c->get_children())
  {
    setup_classes(child, depth + 1);
  }
  c->set_max_child(current_tag - 1);
}

// The code generation first pass. Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
  setup_external_functions();
  setup_classes(root(), 0);
}

// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module()
{
  code_constants();

#ifndef LAB2
  // This must be after code_constants() since that emits constants
  // needed by the code() method for expressions
  CgenNode *mainNode = getMainmain(root());
  mainNode->codeGenMainmain();
#endif
  code_main();

#ifdef LAB2
  code_classes(root());
#endif
}

#ifdef LAB2
void CgenClassTable::code_classes(CgenNode *c)
{
  c->code_class();
  for (auto child : c->get_children())
    code_classes(child);
}
#endif

// Create global definitions for constant Cool objects
void CgenClassTable::code_constants()
{
#ifdef LAB2
  stringtable.code_string_table(this);
#endif
}

// Create LLVM entry point. This function will initiate our Cool program
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
  // Define a function main that has no parameters and returns an i32
  llvm::Function *mainFunc = create_llvm_function("main", llvm::Type::getInt32Ty(context), {}, false);

  // Define an entry basic block
  llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(context, "entry", mainFunc);
  builder.SetInsertPoint(entry_bb);

  // Call Main_main(). This returns int for phase 1, Object for phase 2
  llvm::Function *Mainmain = the_module.getFunction("Main_main");
  if (!Mainmain)
  {
    throw std::runtime_error("Function Main_main not defined.");
  }

#ifdef LAB2
  new__class *new_main_class = new new__class(Main);
  llvm::Value *main_class = new_main_class->code(new CgenEnvironment(root()));
  builder.CreateCall(Mainmain, {main_class}, "tmp.0");
#else
  // Lab1
  llvm::Constant *str = builder.CreateGlobalStringPtr("Main.main() returned %d\n", ".str");

  // Get the address of the string "Main_main() returned %d\n" using getelementptr
  llvm::Value *str_ptr = builder.CreateConstGEP2_32(str->getType(), str, 0, 0, "tmp.1");
  llvm::Value *ret_val = builder.CreateCall(Mainmain, {}, "tmp.0");

  // Call printf with the string address of "Main_main() returned %d\n" and the return value of Main_main() as its arguments
  builder.CreateCall(the_module.getFunction("printf"), {str_ptr, ret_val}, "tmp.2");
#endif
  // Insert return 0
  builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
  auto root = this->find(Object);
  if (!root)
  {
    throw std::runtime_error("Class Object is not defined.");
  }
  return root;
}

#ifndef LAB2
// Special-case functions used for the method Int Main::main() for
// Lab1 only.
CgenNode *CgenClassTable::getMainmain(CgenNode *c)
{
  if (c && !c->basic())
    return c; // Found it!
  for (auto child : c->get_children())
  {
    if (CgenNode *foundMain = this->getMainmain(child))
      return foundMain; // Propagate it up the recursive calls
  }
  return 0; // Make the recursion continue
}
#endif

llvm::Function *CgenClassTable::create_llvm_function(const std::string &funcName,
                                                     llvm::Type *retType,
                                                     llvm::ArrayRef<llvm::Type *> argTypes,
                                                     bool isVarArgs)
{
  assert(retType);
  llvm::FunctionType *ft = llvm::FunctionType::get(retType, argTypes, isVarArgs);
  llvm::Function *func = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, funcName,
                                                this->the_module);
  if (!func)
  {
    if (cgen_debug)
      llvm::errs() << "Function creation failed for function " << funcName;
    llvm_unreachable("Function creation failed");
  }
  return func;
}

/*********************************************************************

  StrTable / IntTable methods

 Coding string, int, and boolean constants

 Cool has three kinds of constants: strings, ints, and booleans.
 This section defines code generation for each type.

 All string constants are listed in the global "stringtable" and have
 type stringEntry. stringEntry methods are defined both for string
 constant definitions and references.

 All integer constants are listed in the global "inttable" and have
 type IntEntry. IntEntry methods are defined for Int constant references only.

 Since there are only two Bool values, there is no need for a table.
 The two booleans are represented by instances of the class BoolConst,
 which defines the definition and reference methods for Bools.

*********************************************************************/

// Create definitions for all String constants
void StrTable::code_string_table(CgenClassTable *ct)
{
  stringtable.add_string("");
  for (auto &[_, entry] : this->_table)
    entry.code_def(ct);
}

// generate code to define a global string constant
void StringEntry::code_def(CgenClassTable *ct)
{
#ifdef LAB2
  llvm::Constant *str = llvm::ConstantDataArray::getString(ct->context, this->get_string(), true);
  llvm::GlobalVariable *str_global = new llvm::GlobalVariable(ct->the_module, str->getType(), true, llvm::GlobalValue::ExternalLinkage, str, '_' + this->get_string() + "_str");
  llvm::Constant *str_vtable = ct->the_module.getNamedGlobal(ct->find(String)->get_vtable_name());
  llvm::Constant *str_obj = llvm::ConstantStruct::get(ct->get_struct_type(ct->find(String)->get_type_name()), str_vtable, str_global);
  llvm::GlobalVariable *str_global_obj = new llvm::GlobalVariable(ct->the_module, str_obj->getType(), true, llvm::GlobalValue::ExternalLinkage, str_obj, '_' + this->get_string() + "_str_obj");
  this->value = str_global_obj;
#endif
}

/*********************************************************************

  CgenNode methods

*********************************************************************/

//
// Class setup. You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).
//
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
  this->tag = tag;
#ifdef LAB2
  layout_features();
#endif
}

#ifdef LAB2
// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
  if (SELF_TYPE == this->get_name())
    return;
  if (cgen_debug)
    llvm::errs() << "Laying out features for class " << this->get_type_name() << "\n";

  this->env = new CgenEnvironment(this);
  llvm::Module &the_module = this->get_classtable()->the_module;

  llvm::StructType *struct_type = this->get_classtable()->get_struct_type(this->get_type_name());
  std::vector<llvm::Type *> struct_types;
  struct_types.push_back(llvm::Type::getInt8PtrTy(this->env->context));
  int num_attributes = the_module.getDataLayout().getTypeAllocSize(struct_types.back()), index = 1;
  for (auto feature : this->features)
    if (auto attr = dynamic_cast<attr_class *>(feature))
    {
      llvm::Type *attr_type = attr->layout_feature(this);
      struct_types.push_back(attr_type);
      this->attributes[attr->get_name()] = {index++, attr_type};
      num_attributes += the_module.getDataLayout().getTypeAllocSize(attr_type);
      if (cgen_debug)
        llvm::errs() << "Attribute " << attr->get_name()->get_string() << " has been added to the struct\n";
      for (auto child : this->get_children())
        child->features = Features_class::append(Features_class::single(attr), child->features);
    }
  struct_type->setBody(struct_types);

  this->methods = this->parentnd->methods;
  llvm::StructType *vtable_type = this->get_classtable()->get_struct_type(this->get_vtable_type_name());
  this->vtable_types.push_back(llvm::Type::getInt32Ty(this->env->context));
  this->vtable_types.push_back(llvm::Type::getInt32Ty(this->env->context));
  this->vtable_types.push_back(llvm::Type::getInt8PtrTy(this->env->context));
  this->vtable_methods.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(this->env->context), this->tag));
  this->vtable_methods.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(this->env->context), num_attributes));
  stringtable.add_string(this->get_type_name());
  for (auto feature : this->features)
    if (auto method = dynamic_cast<method_class *>(feature))
    {
      std::string method_string = method->get_name()->get_string();
      std::string method_name = concat_method_name(this, method->get_name());
      bool overridden = false;
      for (auto &[string, name] : this->methods)
        if (method_string == string)
        {
          name = method_name;
          overridden = true;
          break;
        }
      if (!overridden)
        this->methods.push_back({method_string, method_name});
      method->layout_feature(this);
    }
  for (auto [method_string, method_name] : this->methods)
  {
    auto func = the_module.getFunction(method_name);
    this->vtable_types.push_back(func->getType());
    this->vtable_methods.push_back(func);
    if (cgen_debug)
      llvm::errs() << "Method " << method_name << '(' << method_string << ") has been added to the vtable\n";
  }
  vtable_type->setBody(this->vtable_types);
  the_module.getOrInsertGlobal(this->get_vtable_name(), vtable_type);

  std::string init_function_name = this->get_init_function_name();
  llvm::Type *ret_type = env->class_table.get_struct_type(this->get_type_name())->getPointerTo();
  llvm::FunctionType *ft = llvm::FunctionType::get(ret_type, false);
  llvm::Function::Create(ft, llvm::Function::ExternalLinkage, init_function_name, env->the_module);
}

// Class codegen. This should be performed after every class has been setup.
// Generate code for each method of the class.
void CgenNode::code_class()
{
  this->vtable_methods.insert(this->vtable_methods.begin() + 2, this->get_classtable()->the_module.getNamedGlobal('_' + this->get_type_name() + "_str"));
  llvm::GlobalVariable *vtable = this->get_classtable()->the_module.getNamedGlobal(this->get_vtable_name());
  vtable->setInitializer(llvm::ConstantStruct::get(this->get_classtable()->get_struct_type(this->get_vtable_type_name()), vtable_methods));

  // No code generation for basic classes. The runtime will handle that.
  if (basic())
    return;

  this->code_init_function(this->env);
  for (auto feature : this->features)
    if (auto method = dynamic_cast<method_class *>(feature))
      method->code(this->env);
}

void CgenNode::code_init_function(CgenEnvironment *env)
{
  llvm::Function *func = env->the_module.getFunction(this->get_init_function_name());
  llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(env->context, "entry", func);
  env->builder.SetInsertPoint(entry_bb);

  llvm::StructType *struct_type = env->class_table.get_struct_type(this->get_type_name());
  llvm::Value *obj = env->builder.CreateCall(env->the_module.getFunction("malloc"), {llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), env->the_module.getDataLayout().getTypeAllocSize(struct_type))});
  llvm::Value *vtable = env->the_module.getNamedGlobal(this->get_vtable_name());
  llvm::Value *vtable_ptr = env->builder.CreateStructGEP(struct_type, obj, 0);
  env->builder.CreateStore(vtable, vtable_ptr);
  for (auto feature : this->features)
    if (auto attr = dynamic_cast<attr_class *>(feature))
    {
      llvm::Value *attr_obj = attr->code(env);
      llvm::Value *attr_ptr = env->builder.CreateStructGEP(struct_type, obj, get_layed_out_attr(attr->get_name()).first);
      env->builder.CreateStore(attr_obj, attr_ptr);
    }

  env->builder.CreateRet(obj);
}

#else

// code-gen function main() in class Main
void CgenNode::codeGenMainmain()
{
  // In Phase 1, this can only be class Main. Get method_class for main().
  assert(std::string(this->name->get_string()) == std::string("Main"));
  method_class *mainMethod = (method_class *)features->nth(features->first());

  CgenEnvironment *env = new CgenEnvironment(this);
  mainMethod->code(env);
}

#endif

/*********************************************************************

  CgenEnvironment functions

*********************************************************************/

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t)
{
  return t == SELF_TYPE ? get_class()
                        : get_class()->get_classtable()->find_in_scopes(t);
}

llvm::BasicBlock *CgenEnvironment::get_or_insert_abort_block(llvm::Function *f)
{
  for (auto &bb : *f)
  {
    if (bb.getName() == "abort")
    {
      return &bb;
    }
  }
  auto *abort_bb = llvm::BasicBlock::Create(this->context, "abort", f);
  llvm::Type *void_ = llvm::Type::getVoidTy(this->context);
  llvm::IRBuilder<> builder(abort_bb);
  llvm::FunctionCallee abort = this->the_module.getOrInsertFunction("abort", void_);
  builder.CreateCall(abort, {});
  builder.CreateUnreachable();
  return abort_bb;
}

llvm::AllocaInst *CgenEnvironment::insert_alloca_at_head(llvm::Type *ty)
{
  llvm::BasicBlock &entry_bb = builder.GetInsertBlock()->getParent()->getEntryBlock();
  if (entry_bb.empty())
  {
    // Insert "at the end" of this bb
    return new llvm::AllocaInst(ty, 0, "", &entry_bb);
  }
  else
  {
    // Insert before the first instruction of this bb
    return new llvm::AllocaInst(ty, 0, "", &entry_bb.front());
  }
}

/*********************************************************************

  APS class methods

    Fill in the following methods to produce code for the
    appropriate expression. You may add or remove parameters
    as you wish, but if you do, remember to change the parameters
    of the declarations in `cool-tree.handcode.h'.

*********************************************************************/

void program_class::cgen(const std::optional<std::string> &outfile)
{
  initialize_constants();
  class_table = new CgenClassTable(classes);
  if (outfile)
  {
    std::error_code err;
    llvm::raw_fd_ostream s(*outfile, err, llvm::sys::fs::FA_Write);
    if (err)
    {
      std::cerr << "Cannot open output file " << *outfile << std::endl;
      exit(1);
    }
    s << class_table->the_module;
  }
  else
  {
    llvm::outs() << class_table->the_module;
  }
}

// Create a method body
llvm::Function *method_class::code(CgenEnvironment *env)
{
  auto method_name = concat_method_name(env->get_class(), this->name);
  if (cgen_debug)
  {
    llvm::errs() << "Codegen for method_class: " << method_name << '\n';
  }

  llvm::Function *func = env->the_module.getFunction(method_name);
  llvm::BasicBlock *entry_bb = llvm::BasicBlock::Create(env->context, "entry", func);
  env->builder.SetInsertPoint(entry_bb);

#ifdef LAB2
  env->open_scope();
  auto arg = func->getArg(0);
  llvm::AllocaInst *self_alloca = env->builder.CreateAlloca(arg->getType());
  env->builder.CreateStore(arg, self_alloca);
  env->add_binding(self, self_alloca);
  llvm::Value *self_obj = env->builder.CreateLoad(arg->getType(), self_alloca);
  for (auto feature : env->get_class()->get_features())
    if (auto attr = dynamic_cast<attr_class *>(feature))
    {
      auto attr_name = attr->get_name();
      auto [attr_index, attr_type] = env->get_class()->get_layed_out_attr(attr_name);
      llvm::AllocaInst *alloca = env->builder.CreateAlloca(attr_type);
      llvm::Value *attr_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(env->get_class()->get_type_name()), self_obj, attr_index);
      llvm::Value *attr_obj = env->builder.CreateLoad(attr_type, attr_ptr);
      env->builder.CreateStore(attr_obj, alloca);
      env->add_binding(attr_name, alloca);
    }

  env->open_scope();
  for (int i = 0; i < formals->len(); i++)
  {
    arg = func->getArg(i + 1);
    llvm::AllocaInst *alloca = env->builder.CreateAlloca(arg->getType());
    env->builder.CreateStore(arg, alloca);
    env->add_binding(formals->nth(i)->get_name(), alloca);
  }
#endif

  llvm::Value *value = expr->code(env);

#ifdef LAB2
  env->close_scope();
  env->close_scope();
  if (return_type == SELF_TYPE)
    value = conform(value, env->class_table.get_struct_type(env->get_class()->get_type_name()), env);
  else
    value = conform(value, env->class_table.get_struct_type(return_type->get_string()), env);
#endif
  env->builder.CreateRet(value);
  return func;
}

// Codegen for expressions. Note that each expression has a value.

llvm::Value *assign_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "assign" << '\n';

  llvm::Value *value = conform(expr->code(env), env->find_in_scopes(name)->getAllocatedType(), env);
  env->builder.CreateStore(value, env->find_in_scopes(name));
  return value;
}

llvm::Value *cond_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "cond" << '\n';

  // get value of the condition
  llvm::Value *cond = pred->code(env);

  // get the current function
  llvm::Function *func = env->builder.GetInsertBlock()->getParent();

  // create basic blocks for the then, else, and merge
  llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(env->context, "then", func);
  llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(env->context, "else");
  llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(env->context, "merge");

  // if the condition is true, branch to the then block, else branch to the else block
  env->builder.CreateCondBr(cond, then_bb, else_bb);

  // emit code for the then block
  env->builder.SetInsertPoint(then_bb);
  llvm::Value *then_value = then_exp->code(env);
  if (else_exp->get_type() != then_exp->get_type())
    then_value = conform(then_value, env->class_table.get_struct_type(Object->get_string())->getPointerTo(), env);
  env->builder.CreateBr(merge_bb);
  then_bb = env->builder.GetInsertBlock();

  // emit code for the else block
  func->getBasicBlockList().push_back(else_bb);
  env->builder.SetInsertPoint(else_bb);
  llvm::Value *else_value = else_exp->code(env);
  if (else_exp->get_type() != then_exp->get_type())
    else_value = conform(else_value, env->class_table.get_struct_type(Object->get_string())->getPointerTo(), env);
  env->builder.CreateBr(merge_bb);
  else_bb = env->builder.GetInsertBlock();

  // emit code for the merge block
  func->getBasicBlockList().push_back(merge_bb);
  env->builder.SetInsertPoint(merge_bb);
  llvm::PHINode *phi_node = env->builder.CreatePHI(then_value->getType(), 2, "iftmp"); // TODO: change this for Lab 2
  phi_node->addIncoming(then_value, then_bb);
  phi_node->addIncoming(else_value, else_bb);

  return phi_node;
}

llvm::Value *loop_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "loop" << '\n';

  // get the current function
  llvm::Function *func = env->builder.GetInsertBlock()->getParent();

  // create basic blocks for the predicate, body and exit
  llvm::BasicBlock *pred_bb = llvm::BasicBlock::Create(env->context, "pred", func);
  llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(env->context, "body");
  llvm::BasicBlock *exit_bb = llvm::BasicBlock::Create(env->context, "exit");

  // emit code for the predicate block
  env->builder.CreateBr(pred_bb);
  env->builder.SetInsertPoint(pred_bb);
  llvm::Value *cond = pred->code(env);
  env->builder.CreateCondBr(cond, body_bb, exit_bb);

  // emit code for the body block
  func->getBasicBlockList().push_back(body_bb);
  env->builder.SetInsertPoint(body_bb);
  body->code(env);
  env->builder.CreateBr(pred_bb);

  // emit code for the exit block
  func->getBasicBlockList().push_back(exit_bb);
  env->builder.SetInsertPoint(exit_bb);
  return llvm::ConstantPointerNull::get(env->class_table.get_struct_type(Object->get_string())->getPointerTo());
}

llvm::Value *block_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "block" << '\n';

  llvm::Value *value;
  for (auto expr : body)
    value = expr->code(env);
  return value;
}

llvm::Value *let_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "let" << '\n';

  env->open_scope();
  llvm::Value *init_value = init->code(env);

  if (init_value == nullptr)
  {
    if (env->type_to_class(type_decl)->basic())
    {
      if (type_decl == Int)
        init_value = llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), 0);
      else if (type_decl == Bool)
        init_value = llvm::ConstantInt::get(llvm::Type::getInt1Ty(env->context), 0);
#ifdef LAB2
      else if (type_decl == String)
        init_value = env->the_module.getNamedGlobal("__str_obj");
      else
        init_value = llvm::ConstantPointerNull::get(env->class_table.get_struct_type(type_decl->get_string())->getPointerTo());
#endif
    }
#ifdef LAB2
    else
      init_value = llvm::ConstantPointerNull::get(env->class_table.get_struct_type(type_decl->get_string())->getPointerTo());
#endif
  }
  init_value = conform(init_value, env->class_table.get_struct_type(type_decl->get_string()), env);

  llvm::AllocaInst *alloca = env->builder.CreateAlloca(init_value->getType());
  env->builder.CreateStore(init_value, alloca);
  env->add_binding(identifier, alloca);
  llvm::Value *value = body->code(env);

  env->close_scope();
  return value;
}

llvm::Value *plus_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "plus" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);
  return env->builder.CreateAdd(left, right);
}

llvm::Value *sub_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "sub" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);
  return env->builder.CreateSub(left, right);
}

llvm::Value *mul_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "mul" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);
  return env->builder.CreateMul(left, right);
}

llvm::Value *divide_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "div" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);

  // check if right is zero
  llvm::Value *cmp = env->builder.CreateICmpEQ(right, llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), 0));
  llvm::Function *func = env->builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *abort_bb = env->get_or_insert_abort_block(func);
  llvm::BasicBlock *cont_bb = env->new_bb_at_fend("cont");
  env->builder.CreateCondBr(cmp, abort_bb, cont_bb);
  env->builder.SetInsertPoint(cont_bb);

  return env->builder.CreateSDiv(left, right);
}

llvm::Value *neg_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "neg" << '\n';

  llvm::Value *value = e1->code(env);
  return env->builder.CreateNeg(value);
}

llvm::Value *lt_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "lt" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);
  return env->builder.CreateICmpSLT(left, right);
}

llvm::Value *eq_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "eq" << '\n';

  if (e1->type == Int || e1->type == Bool)
    return env->builder.CreateICmpEQ(e1->code(env), e2->code(env));
  if (e1->type == String)
  {
    llvm::Value *left = env->builder.CreateStructGEP(env->class_table.get_struct_type(String->get_string()), e1->code(env), 1);
    llvm::Value *right = env->builder.CreateStructGEP(env->class_table.get_struct_type(String->get_string()), e2->code(env), 1);
    llvm::Value *value = env->builder.CreateCall(env->the_module.getFunction("strcmp"), {left, right});
    return env->builder.CreateICmpEQ(value, llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), 0));
  }

  llvm::Value *left = env->builder.CreatePtrToInt(e1->code(env), llvm::Type::getInt64Ty(env->context));
  llvm::Value *right = env->builder.CreatePtrToInt(e2->code(env), llvm::Type::getInt64Ty(env->context));
  return env->builder.CreateICmpEQ(left, right);
}

llvm::Value *leq_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "leq" << '\n';

  llvm::Value *left = e1->code(env);
  llvm::Value *right = e2->code(env);
  return env->builder.CreateICmpSLE(left, right);
}

llvm::Value *comp_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "complement" << '\n';

  llvm::Value *value = e1->code(env);
  return env->builder.CreateNot(value);
}

llvm::Value *int_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "Integer Constant" << '\n';

  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), token->get_string(), 10);
}

llvm::Value *bool_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "Boolean Constant" << '\n';

  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(env->context), val);
}

llvm::Value *object_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "Object" << '\n';

  llvm::AllocaInst *alloca = env->find_in_scopes(name);
  return env->builder.CreateLoad(alloca->getAllocatedType(), alloca);
}

llvm::Value *no_expr_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "No_expr" << '\n';

  return nullptr;
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

llvm::Value *static_dispatch_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "static dispatch" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  llvm::Value *value = expr->code(env);
  value = conform(value, env->class_table.get_struct_type(type_name->get_string())->getPointerTo(), env);

  std::string method_name = concat_method_name(env->type_to_class(type_name), name);
  llvm::Function *func = env->the_module.getFunction(method_name);
  std::vector<llvm::Value *> args;
  args.push_back(value);
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    args.push_back(actual->nth(i)->code(env));

  if (cgen_debug)
    llvm::errs() << "Calling function @" << method_name << '\n';
  return env->builder.CreateCall(func, args);
#endif
}

llvm::Value *string_const_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "string_const" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  return env->the_module.getNamedGlobal('_' + token->get_string() + "_str_obj");
#endif
}

llvm::Value *dispatch_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "dispatch" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  CgenNode *cls;
  if (expr->get_type() == SELF_TYPE)
    cls = env->get_class();
  else
    cls = env->type_to_class(this->expr->get_type());

  llvm::Value *value = expr->code(env);
  value = conform(value, env->class_table.get_struct_type(cls->get_type_name())->getPointerTo(), env);

  llvm::Value *vtable_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(cls->get_name()->get_string()), value, 0);
  std::string vtable_type_name = cls->get_vtable_type_name();
  llvm::Value *vtable = env->builder.CreateLoad(env->class_table.get_struct_type(vtable_type_name)->getPointerTo(), vtable_ptr);
  auto [index, method_type] = env->type_to_class(expr->get_type())->get_method_offset_and_type(name);
  llvm::Value *method_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(vtable_type_name), vtable, index);
  llvm::Value *method = env->builder.CreateLoad(method_type, method_ptr);

  std::vector<llvm::Value *> args;
  args.push_back(value);
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
    args.push_back(actual->nth(i)->code(env));

  std::string method_name = cls->get_method_name(name);
  if (cgen_debug)
    llvm::errs() << "Calling function " << method_name << '\n';
  llvm::FunctionType *function_type = env->the_module.getFunction(method_name)->getFunctionType();
  return env->builder.CreateCall(function_type, method, args);
#endif
}

// Handle a Cool case expression (selecting based on the type of an object)
llvm::Value *typcase_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "typecase::code()" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return nullptr`
  return nullptr;
#endif
}

llvm::Value *new__class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "newClass" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  llvm::Function *init_function = env->the_module.getFunction(env->type_to_class(type_name)->get_init_function_name());
  return env->builder.CreateCall(init_function, {});
#endif
}

llvm::Value *isvoid_class::code(CgenEnvironment *env)
{
  if (cgen_debug)
    llvm::errs() << "isvoid" << '\n';
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return nullptr`
  return nullptr;
#endif
}

// Create the LLVM Function corresponding to this method.
llvm::Type *method_class::layout_feature(CgenNode *cls)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  std::string method_name = concat_method_name(cls, name);
  if (cls->get_classtable()->the_module.getFunction(method_name))
    return cls->get_classtable()->the_module.getFunction(method_name)->getType()->getPointerTo();

  std::vector<llvm::Type *> arg_types;
  arg_types.push_back(cls->get_classtable()->get_struct_type(cls->get_type_name())->getPointerTo());
  for (int i = formals->first(); formals->more(i); i = formals->next(i))
  {
    Symbol type_decl = formals->nth(i)->get_type_decl();
    if (Int == type_decl)
      arg_types.push_back(llvm::Type::getInt32Ty(cls->get_classtable()->context));
    else if (Bool == type_decl)
      arg_types.push_back(llvm::Type::getInt1Ty(cls->get_classtable()->context));
    else
      arg_types.push_back(cls->get_classtable()->get_struct_type(type_decl->get_string())->getPointerTo());
  }
  llvm::Type *ret_type;
  if (SELF_TYPE == return_type)
    ret_type = cls->get_classtable()->get_struct_type(cls->get_type_name())->getPointerTo();
  else if (Int == return_type)
    ret_type = llvm::Type::getInt32Ty(cls->get_classtable()->context);
  else if (Bool == return_type)
    ret_type = llvm::Type::getInt1Ty(cls->get_classtable()->context);
  else
    ret_type = cls->get_classtable()->get_struct_type(return_type->get_string())->getPointerTo();
  llvm::errs() << "Creating function " << method_name << " with return type " << return_type->get_string() << " and argument types: ";
  for (unsigned int i = 1; i < arg_types.size(); i++)
    llvm::errs() << formals->nth(i - 1)->get_type_decl()->get_string() << " ";
  llvm::errs() << "\n";

  llvm::FunctionType *ft = llvm::FunctionType::get(ret_type, arg_types, false);
  return llvm::Function::Create(ft, llvm::Function::ExternalLinkage, method_name, cls->get_classtable()->the_module)->getType()->getPointerTo();
#endif
}

// Handle one branch of a Cool case expression.
// If the source tag is >= the branch tag
// and <= (max child of the branch class) tag,
// then the branch is a superclass of the source.
// See the LAB2 handout for more information about our use of class tags.
llvm::Value *branch_class::code(llvm::Value *expr_val, llvm::Value *tag, llvm::Type *join_type,
                                CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  // TODO: add code here and replace `return nullptr`
  return nullptr;
#endif
}

// Assign this attribute a slot in the class structure
llvm::Type *attr_class::layout_feature(CgenNode *cls)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  if (cgen_debug)
    llvm::errs() << "Laying out attribute " << name->get_string() << " : " << type_decl->get_string() << "\n";
  if (type_decl == prim_int || type_decl == Int)
    return llvm::Type::getInt32Ty(cls->get_classtable()->context);
  else if (type_decl == prim_bool || type_decl == Bool)
    return llvm::Type::getInt1Ty(cls->get_classtable()->context);
  else if (prim_string == type_decl)
    return llvm::Type::getInt8PtrTy(cls->get_classtable()->context);
  else if (SELF_TYPE == type_decl)
    return cls->get_classtable()->get_struct_type(cls->get_type_name())->getPointerTo();
  else
    return cls->get_classtable()->get_struct_type(type_decl->get_string())->getPointerTo();
#endif
}

llvm::Value *attr_class::code(CgenEnvironment *env)
{
#ifndef LAB2
  assert(0 && "Unsupported case for phase 1");
#else
  llvm::Value *value = init->code(env);
  if (value == nullptr)
  {
    if (type_decl == prim_int || type_decl == Int)
      value = llvm::ConstantInt::get(llvm::Type::getInt32Ty(env->context), 0);
    else if (type_decl == prim_bool || type_decl == Bool)
      value = llvm::ConstantInt::get(llvm::Type::getInt1Ty(env->context), 0);
    else if (type_decl == String)
      value = env->the_module.getNamedGlobal("__str_obj");
    else
      value = llvm::ConstantPointerNull::get(env->class_table.get_struct_type(type_decl->get_string())->getPointerTo());
  }
  return value;
#endif
}

#ifdef LAB2
llvm::Value *conform(llvm::Value *src, llvm::Type *dest_type, CgenEnvironment *env)
{
  if ((src->getType()->isPointerTy() ^ dest_type->isPointerTy()) == 0)
    return src;

  if (src->getType() == llvm::Type::getInt32Ty(env->context))
  {
    llvm::Function *int_new = env->the_module.getFunction("Int_new");
    llvm::Value *int_obj = env->builder.CreateCall(int_new, {});
    llvm::Value *int_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(Int->get_string()), int_obj, 1);
    env->builder.CreateStore(src, int_ptr);
    return int_obj;
  }

  if (src->getType() == llvm::Type::getInt1Ty(env->context))
  {
    llvm::Function *bool_new = env->the_module.getFunction("Bool_new");
    llvm::Value *bool_obj = env->builder.CreateCall(bool_new, {});
    llvm::Value *bool_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(Bool->get_string()), bool_obj, 1);
    env->builder.CreateStore(src, bool_ptr);
    return bool_obj;
  }

  if (dest_type == llvm::Type::getInt32Ty(env->context))
  {
    llvm::Value *int_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(Int->get_string()), src, 1);
    return env->builder.CreateLoad(llvm::Type::getInt32Ty(env->context), int_ptr);
  }

  if (dest_type == llvm::Type::getInt1Ty(env->context))
  {
    llvm::Value *bool_ptr = env->builder.CreateStructGEP(env->class_table.get_struct_type(Bool->get_string()), src, 1);
    return env->builder.CreateLoad(llvm::Type::getInt1Ty(env->context), bool_ptr);
  }

  return src;
}
#endif
