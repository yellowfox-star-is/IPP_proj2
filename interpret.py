# ========================================================================
# Author: Václav Sysel
# Last update: 21. April 2021
# Name: interpret.py
# Description:
# School project for subject IPP
# School year 2020/2021
# Interpreter for academical language IPPcode21
# IPPcode21 is assembler like interpreted language, which resembles Python
# interpret.py takes XML formatted text,
# checks its validity, creates auxiliary objects and interprets code
# ========================================================================
import argparse
import enum
import copy
import os.path
import re
import sys
import xml.etree.ElementTree
from xml.etree.ElementTree import ElementTree, Element

argument_parser = argparse.ArgumentParser(description='Interprets parsed XML of language IPPcode21')
argument_parser.add_argument('--source', '-s', metavar='file', dest='source_file',
                             help='File from which parsed XML will be loaded.')
argument_parser.add_argument('--input', '-i', metavar='file', dest='input_file',
                             help='File from which input will be loaded in case of READ instruction.')

# global variable used for tracking, when program detects an error
last_executed = -1


# converts python match objects to bool
def match_to_bool(match):
    return match is not None


# expects string, returns true / false, if the string is a valid id
def is_id(string):
    return match_to_bool(re.search("^([-_$&%*!?]|[a-z]|[A-Z])([-_$&%*!?]|[a-z]|[A-Z]|[0-9])*$", string))


# expects object Argument, returns true / false, if Argument is label
def is_label(argument):
    return argument.type == "label" and is_id(argument.value)


# expects object Argument, returns true / false
def is_var(argument):
    if argument.type != "var" or len(argument.value) <= 0:
        return False
    return match_to_bool(re.search("^(LF|TF|GF)@", argument.value)) and is_id(argument.value[3:])


# expects object Argument, returns true / false
def is_nil(argument):
    return argument.type == 'nil' and match_to_bool(re.search("^nil$", argument.value))


# expects object Argument, returns true / false
def is_bool(argument):
    return argument.type == 'bool' and match_to_bool(re.search("^(true|false)$", argument.value))


# expects object Argument, returns true / false
def is_number(argument):
    return argument.type == 'int' and match_to_bool(re.search("^[+-]?[0-9]+$", argument.value))


# expects object Argument
# checks for strings as is written in the assignment (before saved into variable)
# returns true / false
def is_string_lit(argument):
    return argument.type == 'string' and match_to_bool(re.search("^([^\s#\\\\]|\\\\\d{3})*$", argument.value))


# expects object Argument
# checks for internal representations of strings (after saved into variable)
# returns true / false
def is_string_var(argument):
    return argument.type == 'string' and isinstance(argument.value, str)


# expects objcet Argument
# checks for both representations of strings
def is_string(argument):
    return is_string_lit(argument) or is_string_var(argument)


# compound function of previous functions
# expects object Argument
def is_const(argument):
    return is_nil(argument) or is_bool(argument) or is_number(argument) or is_string_lit(argument)


# expects object Argument, retunrs true / false
def is_symb(argument):
    return is_const(argument) or is_var(argument)


# expects object Argument, retunrs true / false
def is_type(argument):
    return argument.type == "type" and match_to_bool(re.search("^(int|bool|string|nil)$", argument.value))


# converts representation of strings as is written in assignment into internal representation
def str_lit_to_var(data: str):
    while True:
        match = re.search("\\\\\d{3}", data)
        if match is None:
            break
        match = match.group()
        try:
            new = chr(int(match[1:]))
        except (ValueError, TypeError, OverflowError):
            return None
        data = data.replace(match, new)
    return data


# contains all error codes and quick strings that are written with them
class Error:
    enums = enum.Enum('error_codes',
                      'IMPLEMENTATION_ERROR INTERNAL_ERROR BAD_ARGUMENT BAD_INPUT_FILE BAD_OUTPUT_FILE '
                      'BAD_XML '
                      'UNEXPECTED_XML '
                      'BAD_SEMANTIC BAD_OPERAND_TYPE NONEXISTENT_VARIABLE '
                      'NONEXISTENT_FRAME MISSING_VALUE BAD_OPERAND_VALUE '
                      'BAD_STRING_OPERATION', start=1)
    strings = {
        # implementation error should newer happen during use of program
        # if yes, some case was overlooked
        enums.IMPLEMENTATION_ERROR: 'So, I have a good message and a good message.'
                                    'The good message is that fault is not on your machine.'
                                    'The bad message is, that the programmer who created this, did something horrible.'
                                    'Please feel free to contact them and tell them, what happened.',

        enums.INTERNAL_ERROR: 'An internal error happened during interpretation. Try again or contact author.',
        enums.BAD_ARGUMENT: 'There is a missing argument, or used combination of arguments is not allowed.',
        enums.BAD_INPUT_FILE: 'An error happened during opening of an input file.',
        enums.BAD_OUTPUT_FILE: 'An error happened during opening of an output file.',
        enums.BAD_XML: 'Input XML isn\'t well-formed.',
        enums.UNEXPECTED_XML: 'An unexpected structure was found in the input XML file. Unable to able to parse XML.',
        enums.BAD_SEMANTIC: 'An error was found during semantic control.',
        enums.BAD_OPERAND_TYPE: 'Instruction used an illegal operand type.',
        enums.NONEXISTENT_VARIABLE: 'The used variable doesn\'t exists.',
        enums.NONEXISTENT_FRAME: 'The used frame doesn\'t exists.',
        enums.MISSING_VALUE: 'Wanted value doesn\'t exists.',
        enums.BAD_OPERAND_VALUE: 'Instruction used illegal operand value.',
        enums.BAD_STRING_OPERATION: 'The used string operation is illegal.',
    }

    codes = {
        # the 666 is here for joke, because if an implementation error happened, something is seriously bad
        enums.IMPLEMENTATION_ERROR: 666,

        # exit codes as written in assignment
        enums.INTERNAL_ERROR: 99,
        enums.BAD_ARGUMENT: 10,
        enums.BAD_INPUT_FILE: 11,
        enums.BAD_OUTPUT_FILE: 12,
        enums.BAD_XML: 31,
        enums.UNEXPECTED_XML: 32,
        enums.BAD_SEMANTIC: 52,
        enums.BAD_OPERAND_TYPE: 53,
        enums.NONEXISTENT_VARIABLE: 54,
        enums.NONEXISTENT_FRAME: 55,
        enums.MISSING_VALUE: 56,
        enums.BAD_OPERAND_VALUE: 57,
        enums.BAD_STRING_OPERATION: 58,
    }

    # if someone would like error messages to be printed into a different place, this is all that need editing
    # print for printing error messaged
    @staticmethod
    def print(string):
        print(string, file=sys.stderr)

    # expects enum from before or string to be printed
    @staticmethod
    def exit(error_enum=None, string=None):
        Error.print('Failed on instruction no: ' + str(last_executed))
        if string is not None:
            Error.print(string)
        else:
            if error_enum is not None:
                Error.print(Error.strings[error_enum])
                exit(Error.codes[error_enum])
        exit(0)


# used for saving arguments from xml
# object representation of Argument
class Argument:
    def __init__(self, argument_type, value):
        self.type = argument_type
        if self.type == 'string':
            if value is None:
                value = ''
        self.value = value


# variables actually saves data the same way as they would be saved in Argument
# but using Argument for saving data would be confusing
# because of this, keyword Data references Argument
# this way data are stored the same way, but the code is little bit more  easily understandable
Data = Argument


# stack implemented with methods
# a list could be used instead of this, but this implementation is protected when stack is empty
class Stack:
    def __init__(self):
        self.list = []

    def push(self, item):
        self.list.append(item)

    def pop(self):
        if not self.empty():
            return self.list.pop()
        else:
            return None

    def top(self):
        return self.list[-1]

    def empty(self):
        return not self.list

    def size(self):
        return len(self.list)


# object representation of instruction
class Instruction:
    def __init__(self, opcode, order):
        self.opcode = opcode
        self.order = order
        self.arguments = []

    def add_argument(self, argument_type, value):
        self.arguments.append(Argument(argument_type=argument_type, value=value))


# frame used for storing variables
# uses dictionary with additional methods
class Frame:
    def __init__(self, initialization=False):
        self.dictionary = {}
        self.is_initialized = initialization

    def check_initialization(self):
        if not self.is_initialized:
            Error.exit(Error.enums.NONEXISTENT_FRAME)

    def set(self, name):
        # name of variable is saved without leading "LF@"
        # if leading "LF@" would be saved,
        # there would have to be a convertor
        # when Local frame changes to Temporary
        name = name[3:]
        self.check_initialization()
        if name in self.dictionary:
            Error.exit(Error.enums.BAD_SEMANTIC)
        self.dictionary[name] = Variable(name)

    def get(self, name):
        name = name[3:]
        self.check_initialization()
        if name not in self.dictionary:
            Error.exit(Error.enums.NONEXISTENT_VARIABLE)
        return self.dictionary[name]


# object representation of variable
class Variable:
    def __init__(self, name):
        self.name = name
        self.value = None
        self.type = None

    def get(self):
        if self.value is None:
            Error.exit(Error.enums.MISSING_VALUE)
        return Data(copy.deepcopy(self.type), copy.deepcopy(self.value))

    def set(self, argument):
        # data are deepcopied out of argument for safety
        self.type = copy.deepcopy(argument.type)
        self.value = copy.deepcopy(argument.value)


# object representation of jump table
# stores names of labels and position where to jump
class JumpTable:
    def __init__(self):
        self.dictionary = {}

    def get(self, name):
        if name not in self.dictionary:
            Error.exit(Error.enums.BAD_SEMANTIC)
        return self.dictionary[name]

    def set(self, name, true_order):
        if name in self.dictionary:
            Error.exit(Error.enums.BAD_SEMANTIC)
        self.dictionary[name] = copy.deepcopy(true_order)


# main object of whole program
class Interpreter:
    root: Element
    tree: ElementTree

    def __init__(self, input_argument=None, source_argument=None):
        self.input_argument = input_argument
        self.source_argument = source_argument
        self.instructions = []
        self.GF = Frame(True)
        self.LF = Stack()
        self.TF = Frame()
        self.current_instruction = -1
        self.position_stack = Stack()
        self.jump_table = JumpTable()
        self.data_stack = Stack()
        self.exit_code = 0

        # files are automatically opened and automatically closed
        try:
            if self.input_argument is not None:
                self.input_file = open(input_argument, 'r')
            else:
                self.input_file = sys.stdin
            if self.source_argument is not None:
                self.source_file = open(source_argument, 'r')
            else:
                self.source_file = sys.stdin
        except OSError:
            Error.exit(Error.enums.BAD_INPUT_FILE)

    def __del__(self):
        if not self.input_file == sys.stdin:
            self.input_file.close()
        if not self.source_file == sys.stdin:
            self.source_file.close()

    # expects valid name of variable
    # returns stack, where the variable should be
    def get_frame(self, variable_name):
        frame_name = variable_name[:2]
        frame = None
        if frame_name == 'LF':
            if self.LF.empty():
                Error.exit(Error.enums.NONEXISTENT_FRAME)
            frame = self.LF.top()
        if frame_name == 'GF':
            frame = self.GF
        if frame_name == 'TF':
            frame = self.TF
        return frame

    # expects valid name of variable
    # returns desired Variable
    def get_var(self, name):
        return self.get_frame(name).get(name)

    # checks self.current_instruction and returns instruction
    # expects valid range of current_instruction
    def get_current_instruction(self):
        return self.instructions[self.current_instruction]

    # checks number of operands in current instruction
    # exits program if wrong
    def check_arguments_sum(self, number):
        if len(self.get_current_instruction().arguments) != number:
            Error.exit(Error.enums.BAD_OPERAND_TYPE)

    # checks argument on desired position
    # expects function, which takes one argument class Argument
    # expects valid position
    def check_argument(self, function, position):
        if not function(self.get_current_instruction().arguments[position]):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)

    # returns argument at position of currently used function
    # expects valid range
    def get_argument(self, position):
        return self.get_current_instruction().arguments[position]

    # gets argument of current instruction at desired position
    # expects valid position
    # returns argument from xml, or data saved in variable named in position
    def get_symb(self, position):
        argument = self.get_argument(position)
        if is_const(argument):
            if is_string_lit(argument):
                argument.value = str_lit_to_var(argument.value)
                if argument.value is None:
                    Error.exit(Error.enums.UNEXPECTED_XML)
            return argument
        if is_var(argument):
            variable = self.get_var(argument.value)
            if variable.type is None:
                Error.exit(Error.enums.MISSING_VALUE)
            return Data(variable.type, variable.value)

    # expects valid position
    # returns variable named on the position
    def prepare_var(self, position):
        return self.get_var(self.get_argument(position).value)

    def operation_preparation(self, test_function=None):
        self.check_arguments_sum(3)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)
        self.check_argument(is_symb, 2)
        a = self.get_symb(1)
        b = self.get_symb(2)
        if test_function is not None:
            if not (test_function(a) and test_function(b)):
                Error.exit(Error.enums.BAD_OPERAND_TYPE)
        return self.prepare_var(0), a, b

    # arithmetics, relation, boolean and conversion instructions
    def check_const(self, test_func, position):
        argument = self.get_argument(position)
        if is_const(argument):
            if not test_func(argument):
                Error.exit(Error.enums.BAD_OPERAND_TYPE)

    # standard function for checking 3 argument operations
    # expects test_func1 and test_func2 to be function, which takes object Argument and return bool
    def operation_check(self, test_func1=None, test_func2=None):
        self.check_arguments_sum(3)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)
        self.check_argument(is_symb, 2)
        if test_func1 is not None:
            self.check_const(test_func1, 1)
        if test_func2 is not None:
            self.check_const(test_func2, 2)

    # version of operation_check for arithmetic operations
    def arithmetic_check(self):
        self.operation_check(is_number, is_number)

    # converts Data (Argument) into values usable for comparator function
    # expects valid Argument
    @staticmethod
    def comp_conversion(a: Data):
        if a.type == 'int':
            return int(a.value)
        if a.type == 'bool':
            if a.value == 'true':
                return 1
            if a.value == 'false':
                return 0

    # makes comparisons between Data
    # expects Data to be valid
    # expects comp_type to be string '>', '<' or '='
    def comparator(self, a: Data, b: Data, comp_type):
        comp_types = ['<', '>', '=']
        if comp_type not in comp_types:
            Error.print('Implementation error in function "comparator"')
            Error.exit(Error.enums.IMPLEMENTATION_ERROR)
        if is_nil(a) or is_nil(b):
            if comp_type == '=':
                return a.value == b.value
            else:
                Error.exit(Error.enums.BAD_OPERAND_TYPE)
        elif a.type != b.type:
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        if a.type == 'string':
            a = a.value
            b = b.value
        else:
            a = self.comp_conversion(a)
            b = self.comp_conversion(b)
        if comp_type == '<':
            return a < b
        elif comp_type == '>':
            return a > b
        elif comp_type == '=':
            return a == b
        else:
            Error.print('Implementation error in function "comparator"')
            Error.exit(Error.enums.IMPLEMENTATION_ERROR)

    # version of operation_check used for comparison instructions
    def comparison_check(self):
        self.operation_check()

    # version of operation_check used for logical instructions
    def logical_check(self):
        self.operation_check(is_bool, is_bool)

    # ===================================================================================
    # INSTRUCTION FUNCTIONS
    #
    # code representation of instructions from IPPcode21
    # for more details viz. assignment

    # frames and calling
    def MOVE_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)

    def MOVE(self):
        self.prepare_var(0).set(self.get_symb(1))

    def CREATEFRAME_test(self):
        self.check_arguments_sum(0)

    def CREATEFRAME(self):
        self.TF = Frame(True)

    def PUSHFRAME_test(self):
        self.check_arguments_sum(0)

    def PUSHFRAME(self):
        if not self.TF.is_initialized:
            Error.exit(Error.enums.NONEXISTENT_FRAME)
        self.LF.push(self.TF)
        self.TF = Frame(False)

    def POPFRAME_test(self):
        self.check_arguments_sum(0)

    def POPFRAME(self):
        if self.LF.empty():
            Error.exit(Error.enums.NONEXISTENT_FRAME)
        self.TF = self.LF.pop()

    def DEFVAR_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_var, 0)

    def DEFVAR(self):
        var_name = self.get_argument(0).value
        frame = self.get_frame(var_name)
        frame.set(var_name)

    def CALL_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_label, 0)

    def CALL(self):
        self.position_stack.push(copy.deepcopy(self.current_instruction))
        self.current_instruction = self.jump_table.get(self.get_argument(0).value)

    def RETURN_test(self):
        self.check_arguments_sum(0)

    def RETURN(self):
        target_location = self.position_stack.pop()
        if target_location is None:
            Error.exit(Error.enums.MISSING_VALUE)
        self.current_instruction = target_location

    # data stack
    def PUSHS_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_symb, 0)

    def PUSHS(self):
        self.data_stack.push(self.get_symb(0))

    def POPS_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_var, 0)

    def POPS(self):
        popped_value = self.data_stack.pop()
        if popped_value is None:
            Error.exit(Error.enums.MISSING_VALUE)
        variable = self.prepare_var(0)
        variable.set(popped_value)

    def ADD_test(self):
        self.arithmetic_check()

    def ADD(self):
        variable, a, b = self.operation_preparation(is_number)
        variable.set(Data('int', str(int(a.value) + int(b.value))))

    def SUB_test(self):
        self.arithmetic_check()

    def SUB(self):
        variable, a, b = self.operation_preparation(is_number)
        variable.set(Data('int', str(int(a.value) - int(b.value))))

    def MUL_test(self):
        self.arithmetic_check()

    def MUL(self):
        variable, a, b = self.operation_preparation(is_number)
        variable.set(Data('int', str(int(a.value) * int(b.value))))

    def IDIV_test(self):
        self.arithmetic_check()

    def IDIV(self):
        variable, a, b = self.operation_preparation(is_number)
        if int(b.value) == 0:
            Error.exit(Error.enums.BAD_OPERAND_VALUE)
        variable.set(Data('int', str(int(a.value) // int(b.value))))

    def LT_test(self):
        self.comparison_check()

    def LT(self):
        variable, a, b = self.operation_preparation()
        variable.set(Data('bool', str.lower(str(self.comparator(a, b, '<')))))

    def GT_test(self):
        self.comparison_check()

    def GT(self):
        variable, a, b = self.operation_preparation()
        variable.set(Data('bool', str.lower(str(self.comparator(a, b, '>')))))

    def EQ_test(self):
        self.comparison_check()

    def EQ(self):
        variable, a, b = self.operation_preparation()
        variable.set(Data('bool', str.lower(str(self.comparator(a, b, '=')))))

    def AND_test(self):
        self.logical_check()

    def AND(self):
        variable, a, b = self.operation_preparation(is_bool)
        a = bool(self.comp_conversion(a))
        b = bool(self.comp_conversion(b))
        variable.set(Data('bool', str.lower(str(a and b))))

    def OR_test(self):
        self.logical_check()

    def OR(self):
        variable, a, b = self.operation_preparation(is_bool)
        a = bool(self.comp_conversion(a))
        b = bool(self.comp_conversion(b))
        variable.set(Data('bool', str.lower(str(a or b))))

    def NOT_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)
        self.check_const(is_bool, 1)

    def NOT(self):
        variable = self.prepare_var(0)
        a = self.get_symb(1)
        if not is_bool(a):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        variable.set(Data('bool', str.lower(str(not bool(self.comp_conversion(a))))))

    def INT2CHAR_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)
        self.check_const(is_number, 1)

    def INT2CHAR(self):
        variable = self.prepare_var(0)
        a = self.get_symb(1)
        if not is_number(a):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        try:
            result = chr(int(a.value))
        except (ValueError, TypeError, OverflowError):
            Error.exit(Error.enums.BAD_STRING_OPERATION)
        variable.set(Data('string', result))

    def STRI2INT_test(self):
        self.operation_check(is_string, is_number)

    def STRI2INT(self):
        variable, a, b = self.operation_preparation()
        if not (is_string(a) and is_number(b)):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        a = a.value
        b = int(b.value)
        if not b < len(a) or b < 0:
            Error.exit(Error.enums.BAD_STRING_OPERATION)
        variable.set(Data('int', ord(a[b])))

    def READ_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_type, 1)

    def READ(self):
        variable = self.prepare_var(0)
        var_type = self.get_argument(1)
        input_data = self.input_file.readline()
        if len(input_data) != 0:
            if input_data[-1] == '\n':
                input_data = input_data[:-1]
            if var_type.value == 'int':
                result = Data('int', input_data)
                if is_number(result):
                    variable.set(result)
                    return
            if var_type.value == 'string':
                result = Data('string', input_data)
                if is_string(result):
                    result.value = str_lit_to_var(result.value)
                    variable.set(result)
                    return
            if var_type.value == 'bool':
                if str.lower(input_data) == 'true':
                    result = 'true'
                else:
                    result = 'false'
                variable.set(Data('bool', result))
                return
        variable.set(Data('nil', 'nil'))
        return

    @staticmethod
    def WRITE_print(string):
        print(string, end='')

    @staticmethod
    def WRITE_conversion(data):
        if is_bool(data):
            if str.lower(data.value) == 'true':
                return 'true'
            if str.lower(data.value) == 'false':
                return 'false'
        elif is_nil(data):
            return ''
        else:
            return str(data.value)

    def WRITE_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_symb, 0)

    def WRITE(self):
        data = self.get_symb(0)
        to_print = self.WRITE_conversion(data)
        self.WRITE_print(to_print)

    def CONCAT_test(self):
        self.operation_check(is_string, is_string)

    def CONCAT(self):
        variable, a, b = self.operation_preparation(is_string)
        variable.set(Data('string', a.value + b.value))

    def STRLEN_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)
        self.check_const(is_string, 1)

    def STRLEN(self):
        variable = self.prepare_var(0)
        a = self.get_symb(1)
        if not is_string(a):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        variable.set(Data('int', str(len(a.value))))

    def GETCHAR_test(self):
        self.operation_check(is_string, is_number)

    def GETCHAR(self):
        variable, a, b = self.operation_preparation()
        if not (is_string(a) and is_number(b)):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        if not int(b.value) < len(a.value) or int(b.value) < 0:
            Error.exit(Error.enums.BAD_STRING_OPERATION)
        variable.set(Data('string', a.value[int(b.value)]))

    def SETCHAR_test(self):
        self.operation_check(is_number, is_string)

    def SETCHAR(self):
        variable, a, b = self.operation_preparation()
        c = variable.get()
        if not (is_number(a) and is_string(b) and is_string(c)):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        if len(b.value) == 0:
            Error.exit(Error.enums.BAD_STRING_OPERATION)
        a, b, c = int(a.value), b.value[0], c.value
        if not (0 <= a < len(c)):
            Error.exit(Error.enums.BAD_STRING_OPERATION)
        variable.set(Data('string', c[:a] + b + c[a + 1:]))

    def TYPE_test(self):
        self.check_arguments_sum(2)
        self.check_argument(is_var, 0)
        self.check_argument(is_symb, 1)

    def TYPE(self):
        variable = self.prepare_var(0)
        argument = self.get_argument(1)
        if argument.type == 'var':
            tested_variable = self.get_var(argument.value)
            if tested_variable.type is None:
                result = ''
            else:
                result = tested_variable.get().type
        else:
            result = argument.type
        variable.set(Data('string', result))

    def LABEL_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_label, 0)

    def LABEL(self):
        self.check_arguments_sum(1)
        self.check_argument(is_label, 0)
        label = self.get_argument(0).value
        self.jump_table.set(label, self.current_instruction)

    def JUMP_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_label, 0)

    def JUMP(self):
        label = self.get_argument(0).value
        self.current_instruction = self.jump_table.get(label)

    def JUMPIFEQ_test(self):
        self.check_arguments_sum(3)
        self.check_argument(is_label, 0)
        self.check_argument(is_symb, 1)
        self.check_argument(is_symb, 2)

    def JUMPIFEQ(self):
        a, b = self.get_symb(1), self.get_symb(2)
        new_position = self.jump_table.get(self.get_argument(0).value)
        to_jump = self.comparator(a, b, '=')
        if to_jump:
            self.current_instruction = new_position

    def JUMPIFNEQ_test(self):
        self.check_arguments_sum(3)
        self.check_argument(is_label, 0)
        self.check_argument(is_symb, 1)
        self.check_argument(is_symb, 2)

    def JUMPIFNEQ(self):
        a, b = self.get_symb(1), self.get_symb(2)
        new_position = self.jump_table.get(self.get_argument(0).value)
        to_jump = not self.comparator(a, b, '=')
        if to_jump:
            self.current_instruction = new_position

    def EXIT_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_symb, 0)

    def EXIT(self):
        exit_code = self.get_symb(0)
        if not is_number(exit_code):
            Error.exit(Error.enums.BAD_OPERAND_TYPE)
        else:
            exit_code = int(exit_code.value)
        if not 0 <= exit_code <= 49:
            Error.exit(Error.enums.BAD_OPERAND_VALUE)
        exit(exit_code)

    def DPRINT_test(self):
        self.check_arguments_sum(1)
        self.check_argument(is_symb, 0)

    def DPRINT(self):
        a = self.get_symb(0)
        to_print = self.WRITE_conversion(a)
        Error.print(to_print)

    def BREAK_test(self):
        pass

    def BREAK(self):
        Error.print('Executed         : ' + str(self.current_instruction) + ' instructions.')
        curr_instr = self.get_current_instruction()
        Error.print('Curr instr opcode: ' + curr_instr.opcode)
        Error.print('Curr instr order : ' + str(curr_instr.order))
        Error.print('Data stack:')
        Error.print(self.data_stack.list)
        Error.print('Position stack:')
        Error.print(self.position_stack.list)
        Error.print('Jump table:')
        Error.print(self.jump_table.dictionary)
        Error.print('LF stack')
        Error.print(self.LF.list)
        Error.print('GF')
        Error.print(self.GF.dictionary)
        Error.print('TF')
        Error.print(self.TF.dictionary)
        Error.print('Current LF')
        Error.print(self.LF.top().dictionary)

    #
    # INSTRUCTION FUNCTIONS ===== END
    # ===================================================================================

    # function which resembles instruction functions but does nothing
    # can be used as a place holder, or if some opcode isn't supposed to be executed
    def DUMMY_INSTRUCTION(self):
        pass

    # looks into dictionary and if valid executes function
    # expects opcode in uppercase from xml
    def execute_function(self, function_name):
        function = Interpreter.dictionary.get(function_name)
        if function is None:
            Error.exit(Error.enums.UNEXPECTED_XML)
        function(self=self)

    # dictionary of all 'sharp' instruction functions
    dictionary = {
        MOVE.__name__: MOVE,
        CREATEFRAME.__name__: CREATEFRAME,
        PUSHFRAME.__name__: PUSHFRAME,
        POPFRAME.__name__: POPFRAME,
        DEFVAR.__name__: DEFVAR,
        CALL.__name__: CALL,
        RETURN.__name__: RETURN,

        PUSHS.__name__: PUSHS,
        POPS.__name__: POPS,

        ADD.__name__: ADD,
        SUB.__name__: SUB,
        MUL.__name__: MUL,
        IDIV.__name__: IDIV,
        LT.__name__: LT,
        GT.__name__: GT,
        EQ.__name__: EQ,
        AND.__name__: AND,
        OR.__name__: OR,
        NOT.__name__: NOT,
        INT2CHAR.__name__: INT2CHAR,
        STRI2INT.__name__: STRI2INT,

        READ.__name__: READ,
        WRITE.__name__: WRITE,

        CONCAT.__name__: CONCAT,
        STRLEN.__name__: STRLEN,
        GETCHAR.__name__: GETCHAR,
        SETCHAR.__name__: SETCHAR,

        TYPE.__name__: TYPE,

        LABEL.__name__: DUMMY_INSTRUCTION,
        JUMP.__name__: JUMP,
        JUMPIFEQ.__name__: JUMPIFEQ,
        JUMPIFNEQ.__name__: JUMPIFNEQ,
        EXIT.__name__: EXIT,

        DPRINT.__name__: DPRINT,
        BREAK.__name__: BREAK,
    }

    # dictionary of all instruction functions used for testing before interpretation
    pre_interpretation_dictionary = {
        MOVE_test.__name__: MOVE_test,
        CREATEFRAME_test.__name__: CREATEFRAME_test,
        PUSHFRAME_test.__name__: PUSHFRAME_test,
        POPFRAME_test.__name__: POPFRAME_test,
        DEFVAR_test.__name__: DEFVAR_test,
        CALL_test.__name__: CALL_test,
        RETURN_test.__name__: RETURN_test,

        PUSHS_test.__name__: PUSHS_test,
        POPS_test.__name__: POPS_test,

        ADD_test.__name__: ADD_test,
        SUB_test.__name__: SUB_test,
        MUL_test.__name__: MUL_test,
        IDIV_test.__name__: IDIV_test,
        LT_test.__name__: LT_test,
        GT_test.__name__: GT_test,
        EQ_test.__name__: EQ_test,
        AND_test.__name__: AND_test,
        OR_test.__name__: OR_test,
        NOT_test.__name__: NOT_test,
        INT2CHAR_test.__name__: INT2CHAR_test,
        STRI2INT_test.__name__: STRI2INT_test,

        READ_test.__name__: READ_test,
        WRITE_test.__name__: WRITE_test,

        CONCAT_test.__name__: CONCAT_test,
        STRLEN_test.__name__: STRLEN_test,
        GETCHAR_test.__name__: GETCHAR_test,
        SETCHAR_test.__name__: SETCHAR_test,

        TYPE_test.__name__: TYPE_test,

        LABEL_test.__name__: LABEL,
        JUMP_test.__name__: JUMP_test,
        JUMPIFEQ_test.__name__: JUMPIFEQ_test,
        JUMPIFNEQ_test.__name__: JUMPIFNEQ_test,
        EXIT_test.__name__: EXIT_test,

        DPRINT_test.__name__: DPRINT_test,
        BREAK_test.__name__: BREAK_test,
    }

    # checks if files exists
    # expects arguments from argparse
    @staticmethod
    def validate_arguments(arguments):
        if not (arguments.source_file or arguments.input_file):
            Error.exit(Error.enums.BAD_ARGUMENT)
        if arguments.source_file is not None:
            if not os.path.isfile(arguments.source_file):
                Error.exit(Error.enums.BAD_INPUT_FILE)
        if arguments.input_file is not None:
            if not os.path.isfile(arguments.input_file):
                Error.exit(Error.enums.BAD_INPUT_FILE)

    # loads tree and saves it into interpret
    # expects opened source file in interpret
    def xml_load_tree(self):
        self.tree = xml.etree.ElementTree.parse(self.source_file)
        self.root = self.tree.getroot()

    # checks if xml structure is okay
    # expects loaded tree root (xml_load_tree used before)
    def check_xml(self):
        if self.root.tag != 'program':
            Error.exit(Error.enums.UNEXPECTED_XML)
        used_instruction_order = []
        for child in self.root:
            if child.tag != 'instruction':
                Error.exit(Error.enums.UNEXPECTED_XML)
            child_attributes = list(child.attrib.keys())
            if not ('order' in child_attributes) or not ('opcode' in child_attributes):
                Error.exit(Error.enums.UNEXPECTED_XML)
            order = child.get('order')
            try:
                if not (order in used_instruction_order) and int(order) >= 1:
                    used_instruction_order.append(order)
                else:
                    Error.exit(Error.enums.UNEXPECTED_XML)
            except ValueError:
                Error.exit(Error.enums.UNEXPECTED_XML)

            for sub_element in child:
                if not (re.match(r"arg[123]", sub_element.tag)):
                    Error.exit(Error.enums.UNEXPECTED_XML)

    # parses xml structure into intern objects
    # expects validated xml structure in root (check_xml used before)
    def parse_xml(self):
        for element in self.root:
            instruction = Instruction(opcode=element.get('opcode'), order=int(element.get('order')))
            for sub_element in element:
                instruction.add_argument(argument_type=sub_element.get('type'), value=sub_element.text)
            self.instructions.append(instruction)
        self.instructions.sort(key=lambda instr: instr.order)

    # does pre-intepretation checks and prepares jump table
    # expects prepared intern objects (parse_xml used before)
    def verify_instructions(self):
        self.current_instruction = 0
        program_end = len(self.instructions)
        while self.current_instruction < program_end:
            curr_instr = self.get_current_instruction()
            if str.upper(curr_instr.opcode) == 'LABEL':
                self.LABEL()
            else:
                function = self.pre_interpretation_dictionary.get(str.upper(curr_instr.opcode) + '_test')
                if function is None:
                    Error.exit(Error.enums.UNEXPECTED_XML)
                else:
                    function(self)
            self.current_instruction += 1
            if self.current_instruction >= program_end:
                break
        self.current_instruction = -1

    # does interpretation of source file
    # expects prepared intern objects, prepared jump table and validated basic checks (verify_instructions used before)
    def interpret(self):
        global last_executed
        if self.current_instruction == -1:
            self.current_instruction = 0
            last_executed = 0
        program_end = len(self.instructions)
        while self.current_instruction < program_end:
            self.execute_function(str.upper(self.instructions[self.current_instruction].opcode))
            self.current_instruction += 1
            last_executed = self.current_instruction
            if self.current_instruction >= program_end:
                break


if __name__ == '__main__':
    arguments = argument_parser.parse_args()
    Interpreter.validate_arguments(arguments)
    interpreter = Interpreter(input_argument=arguments.input_file, source_argument=arguments.source_file)
    interpreter.xml_load_tree()
    interpreter.check_xml()
    interpreter.parse_xml()
    interpreter.verify_instructions()
    interpreter.interpret()
