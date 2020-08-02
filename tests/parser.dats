(* Compile with:
 * patscc -DATS_MEMALLOC_LIBC -o test_parser ../src/lib.dats parser.dats
 *)

#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"
#include "clast/lib.hats"

fun {a: t@ype}{b: t@ype} result_unwrap(result: Result(a, b)): a =
    let val- Ok(value) = result
    in
        value
    end

fun string_list_eq(list1: list0(string), list2: list0(string)): bool =
    list0_equal(list1, list2, lam(val1, val2) => geq_val_val<string>(val1, val2))

fun create_simple_parser(args: List(string)): ArgParser =
    let val arg_parser =
       @{ args = g0ofg1(args)
        , default_command_spec =
            ref(
                @{ parameters = nil0()
                 , short_to_long = hashtbl_make_nil<string, string>(i2sz(10))
                 , description = ""
                 }
            )
        , subcommands = hashtbl_make_nil<string, ref(CommandSpec)>(i2sz(10))
        , description = ref("Clast Test")
        , program_name = ref("clast_test")
        }
        val _ = arg_parser.add_flag(
           '{ short_name = "h"
            , long_name = "help"
            , description = "Show help message"
            }
        )
        val _ = arg_parser.add_flag(
           '{ short_name = "v"
            , long_name = "version"
            , description = "Show version number"
            }
        )
    in
        arg_parser
    end

fun create_parser(args: List(string)): ArgParser =
    let val arg_parser =
       @{ args = g0ofg1(args)
        , default_command_spec =
            ref(
                @{ parameters = nil0()
                 , short_to_long = hashtbl_make_nil<string, string>(i2sz(10))
                 , description = ""
                 }
            )
        , subcommands = hashtbl_make_nil<string, ref(CommandSpec)>(i2sz(10))
        , description = ref("Clast Test")
        , program_name = ref("clast_test")
        }
        val _ = arg_parser.add_flag(
           '{ short_name = "h"
            , long_name = "help"
            , description = "Show help message"
            }
        )
        val _ = arg_parser.add_flag(
           '{ short_name = "v"
            , long_name = "version"
            , description = "Show version number"
            }
        )
        val subcommand = arg_parser.add_subcommand("build", "Compile the project")
        val _ = subcommand.add_arg(
           '{ short_name = "o"
            , long_name = "output"
            , description = "Set output file name"
            , hint = "NAME"
            }
        )
        val _ = subcommand.add_flag(
           '{ short_name = "g"
            , long_name = "debug"
            , description = "Debug mode"
            }
        )
    in
        arg_parser
    end

implement main0(argc, argv) = {
    val arg_parser = create_parser($list{string}("-h"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("-hv"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(matches.get_flag("v"));
        assertloc(matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("-h", "--version"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(matches.get_flag("v"));
        assertloc(matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("-h", "-v"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(matches.get_flag("v"));
        assertloc(matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-o", "test", "-g"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-g", "-o", "test"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("--help"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-g", "--output", "test"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "--output", "test", "--debug"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "--output", "test"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}())
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-g"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-go", "test"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-gotest"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-ogtest"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "gtest");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "gtest");
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("build", "-oghtest"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "ghtest");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "ghtest");
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), nil0()));
    )

    val arg_parser = create_parser($list{string}("-h", "testing"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), list0_sing("testing")));
    )

    val arg_parser = create_parser($list{string}("build", "-o", "test", "-g", "testing"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), list0_sing("testing")));
    )

    val arg_parser = create_parser($list{string}("-h", "testing", "clast"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option0_is_none(matches.get_string("o")));
        assertloc(option0_is_none(matches.get_string("output")));
        assertloc(~matches.get_flag("g"));
        assertloc(~matches.get_flag("debug"));
        assertloc(matches.get_flag("h"));
        assertloc(matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(~matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), g0ofg1($list{string}("testing", "clast"))));
    )

    val arg_parser = create_parser($list{string}("build", "-o", "test", "-g", "testing", "clast"))
    val matches = result_unwrap(arg_parser.parse())
    val () = (
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("o"))) = "test");
        assertloc(option_unsome_exn(g1ofg0(matches.get_string("output"))) = "test");
        assertloc(matches.get_flag("g"));
        assertloc(matches.get_flag("debug"));
        assertloc(~matches.get_flag("h"));
        assertloc(~matches.get_flag("help"));
        assertloc(~matches.get_flag("v"));
        assertloc(~matches.get_flag("version"));
        assertloc(matches.is_subcommand("build"));
        assertloc(string_list_eq(matches.get_free_args(), g0ofg1($list{string}("testing", "clast"))));
    )

    val arg_parser = create_parser($list{string}("build", "-h", "testing"))
    val () =
        case+ arg_parser.parse() of
        | Ok(_) => assertloc(false)
        | Err(error) => assertloc(error_string(error) = "No parameter named h")

    val arg_parser = create_parser($list{string}("-gh", "testing"))
    val () =
        case+ arg_parser.parse() of
        | Ok(_) => assertloc(false)
        | Err(error) => assertloc(error_string(error) = "No parameter named g")

    val arg_parser = create_parser($list{string}("build", "-o"))
    val () =
        case+ arg_parser.parse() of
        | Ok(_) => assertloc(false)
        | Err(error) => assertloc(error_string(error) = "Missing value for parameter output")

    val arg_parser = create_parser($list{string}())
    val _ = arg_parser.add_flag(
       '{ long_name = "list"
        , description = "Show version number"
        }
    )
    val usage = arg_parser_usage_string(arg_parser)
    val () = assertloc(usage = "Clast Test

USAGE:
    clast_test [OPTIONS] [SUBCOMMAND]

OPTIONS:
        --list       Show version number
    -v, --version    Show version number
    -h, --help       Show help message

SUBCOMMANDS:
    build    Compile the project
")

    val usage = result_unwrap(arg_parser_subcommand_usage_string(arg_parser, "build"))
    val () = assertloc(usage = "Clast Test

USAGE:
    clast_test build [OPTIONS]

OPTIONS:
    -g, --debug            Debug mode
    -o, --output <NAME>    Set output file name
")

    val arg_parser = create_simple_parser($list{string}())
    val usage = arg_parser_usage_string(arg_parser)
    val () = assertloc(usage = "Clast Test

USAGE:
    clast_test [OPTIONS]

OPTIONS:
    -v, --version    Show version number
    -h, --help       Show help message
")
}
