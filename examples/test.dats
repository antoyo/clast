(* Compile with:
 * patscc -IATS ../.. -DATS_MEMALLOC_LIBC -o test ../src/lib.dats test.dats
 *)

#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#include "atstd/lib.hats"
#include "clast/lib.hats"

implement main0(argc, argv) = {
    val arg_parser = arg_parser(argc, argv)
    val _ = arg_parser.set_description("Clast Example")
    val _ = arg_parser.set_program_name("clast_example")
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
    val _ = arg_parser.add_flag(
       '{ long_name = "list"
        , description = "Show version number"
        }
    )
    val subcommand = arg_parser.add_subcommand("build", "Compile the current project")
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
    val _ = subcommand.add_flag(
       '{ short_name = "h"
        , long_name = "help"
        , description = "Show help message"
        }
    )
    val subcommand = arg_parser.add_subcommand("help", "Show usage of a subcommand")

    val () =
        case+ arg_parser.parse() of
        | Ok(matches) => {
            val output_file = matches.get_string("o")
            val () = println!("Output file ", output_file)
            val output_file = matches.get_string("output")
            val () = println!("Output file ", output_file)
            val () = println!("Debug ", matches.get_flag("g"))
            val () = println!("Debug ", matches.get_flag("debug"))
            val () = println!("Help ", matches.get_flag("h"))
            val () = println!("Help ", matches.get_flag("help"))
            val () = println!("Version ", matches.get_flag("v"))
            val () = println!("Version ", matches.get_flag("version"))
            val () = println!("Free: ", matches.get_free_args())
            val () = println!("Is build command: ", matches.is_subcommand("build"))
            val () =
                if matches.get_flag("help") then
                    case+ matches.subcommand of
                    | Some0(subcommand) => let val _ = arg_parser.print_usage(subcommand) in end
                    | None0() => arg_parser.print_usage()
        }
        | Err(error) => (
            prerrln!("error: ", error);
            println!();
            arg_parser.print_usage()
        )
}
