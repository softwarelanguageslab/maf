package maf.core

/** Type of an expression. Can be used to distinguish expressions based on their type. */
enum Label:
    // Identifiers
    case SYM // Identifier

    // Scheme
    case BEG // Begin
    case DFV // Variable definition
    case FNC // Function call (application)
    case IFF // If expression
    case LAM // Lambda expression
    case LET // Let expression
    case LTR // Letrec expression
    case LTS // Let* expression
    case PAI // Pair expression
    case VEC // Vector expression
    case SET // Assignment
    case VAL // Value
    case VAR // Variable

    // Assertions
    case ASS // Assertion
