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
    case SET // Assignment
    case VAL // Value
    case VAR // Variable

    // Assertions
    case ASS // Assertion

    // Code changes
    case CHA // Code change

    // Concurrent Scheme
    case FRK // Fork
    case JOI // Join

    // Contract language
    case DFC // define/contract
    case DPC // Dependent contract
    case FLC // Flat contract
    case MON // Monitor
    case CHK // Check expression

    // Racket module system (used in the contract language)
    case PROV // Provide expression
    case PCO // Provide contract-out element

    // Structs
    case MSG // Struct getter
    case MSS // Struct setter
    case MSC // Struct constructor
    case MSP // Struct predicate

    // Match expressions
    case MEX // match expression
