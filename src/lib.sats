staload "libats/ML/SATS/hashtblref.sats"

#include "atstd/lib.hats"

#define ATS_PACKNAME "clast"

datatype Error =
  | MissingValue of String
  | ParamDoesNotExist of String
  | UnknownSubcommand of String

typedef LongFlag =
   '{ long_name = String1
    , description = String0
    }

typedef ShortLongFlag =
   '{ short_name = string(1)
    , long_name = String1
    , description = String0
    }

typedef LongParam =
   '{ short_name = string(1)
    , long_name = String1
    , description = String0
    , hint = String0
    }

typedef Param =
   '{ short_name = Option(string(1))
    , long_name = String1
    , description = String0
    , hint = Option(String0)
    , has_argument = bool
    }

typedef CommandSpec =
   @{ parameters = List0(Param)
    , short_to_long = hashtbl(String, String)
    , description = String
    }

typedef ArgParser =
   @{ args = List0(String0)
    , default_command_spec = ref(CommandSpec)
    , subcommands = hashtbl(String, ref(CommandSpec))
    , description = ref(String)
    , program_name = ref(String)
    }

typedef Matches =
   '{ args = hashtbl(String, Option(String))
    , free_args = List(String)
    , short_to_long = hashtbl(String, String)
    , subcommand = Option(String)
    }

fun arg_parser {n: pos} (argc: int(n), argv: !argv(n)): ArgParser

fun arg_parser_add_arg_long_optional(parser: !ArgParser, arg: LongParam): void

overload .add_arg with arg_parser_add_arg_long_optional

fun command_spec_add_arg_long_optional(spec: ref(CommandSpec), arg: LongParam): void

overload .add_arg with command_spec_add_arg_long_optional

fun arg_parser_add_flag_long(parser: !ArgParser, flag: LongFlag): void

overload .add_flag with arg_parser_add_flag_long

fun command_spec_add_flag_long(spec: ref(CommandSpec), flag: LongFlag): void

overload .add_flag with command_spec_add_flag_long

fun arg_parser_add_flag_short_long(parser: !ArgParser, flag: ShortLongFlag): void

overload .add_flag with arg_parser_add_flag_short_long

fun command_spec_add_flag_short_long(spec: ref(CommandSpec), flag: ShortLongFlag): void

overload .add_flag with command_spec_add_flag_short_long

fun arg_parser_parse(parser: ArgParser): Result(Matches, Error)

overload .parse with arg_parser_parse

fun matches_get_string(matches: Matches, name: String): Option(String)

overload .get_string with matches_get_string

fun matches_get_flag(matches: Matches, name: String): bool

overload .get_flag with matches_get_flag

fun matches_get_free_args(matches: Matches): List(String)

overload .get_free_args with matches_get_free_args

fun matches_is_subcommand(matches: Matches, name: String): bool

overload .is_subcommand with matches_is_subcommand

fun arg_parser_add_subcommand(parser: ArgParser, name: String, description: String): ref(CommandSpec)

overload .add_subcommand with arg_parser_add_subcommand

fun arg_parser_set_description(parser: ArgParser, description: String): void

overload .set_description with arg_parser_set_description

fun arg_parser_set_program_name(parser: ArgParser, name: String): void

overload .set_program_name with arg_parser_set_program_name

fun arg_parser_print_usage(parser: ArgParser): void

overload .print_usage with arg_parser_print_usage

fun arg_parser_usage_string(parser: ArgParser): String

fun arg_parser_print_subcommand_usage(parser: ArgParser, subcommand: String): Result((), Error)

overload .print_usage with arg_parser_print_subcommand_usage

fun arg_parser_subcommand_usage_string(parser: ArgParser, subcommand: String): Result(String, Error)

fun prerr_error(error: Error): void

overload prerr with prerr_error

fun error_string(error: Error): String
