(* TODO: support -- to add free args. *)

#include "share/atspre_staload.hats"

staload "libats/ML//SATS/hashtblref.sats"
staload _ = "libats/ML/DATS/hashtblref.dats"

#define ATS_DYNLOADFLAG 0

#include "atstd/lib.hats"

#define ATS_PACKNAME "clast"

staload "lib.sats"

exception Unreachable (* TODO: remove when code is written with dependent types. *)

val hint_margin_length: Size_t = i2sz(3) (* 2 for " <" + 1 for ">" *)
val minimum_spaces: Size_t = i2sz(4)

typedef Parser =
   '{ args = ref(List0(String0))
    , free_args = ref(List0(String))
    , name_values = hashtbl(String, Option(String))
    , spec = ref(CommandSpec)
    , subcommands = hashtbl(String, ref(CommandSpec))
    }

fun {a, b:t@ype} list_foldleft {n:nat} .<n>. (list: list(a, n), init: b, func: (b, a) -<cloref1> b): b =
    case+ list of
    | list_nil() => init
    | list_cons(x, xs) =>
        list_foldleft(xs, func(init, x), func)

fun {a:t@ype}{b:t@ype} list_map {n:nat} .<n>. (list: list(a, n), func: a -<cloref1> b): list(b, n) =
    let fun {a:t@ype}{b:t@ype} map {n:nat} .<n>. (list: list(a, n), func: a -<cloref1> b): list(b, n) =
        case+ list of
        | list_nil() => nil()
        | list_cons(x, xs) => cons(func(x), map(xs, func))
    in
        list_of_list_vt(list_reverse(map(list, func)))
    end

implement arg_parser (argc, argv) =
    let val args: List(String0) = list_map<string><String0>(
            list_tail_exn(list_of_list_vt(listize_argc_argv(argc, argv))), lam(string) => g1ofg0(string)
        )
    in
       @{ args = args
        , default_command_spec =
            ref(
                @{ parameters = nil()
                 , short_to_long = hashtbl_make_nil<String, String>(i2sz(10))
                 , description = ""
                 }
            )
        , subcommands = hashtbl_make_nil<String, ref(CommandSpec)>(i2sz(10))
        , description = ref("")
        , program_name = ref("")
        }
    end

implement arg_parser_add_arg_long_optional(parser, arg) =
    command_spec_add_arg_long_optional(parser.default_command_spec, arg)

implement command_spec_add_arg_long_optional(spec, arg) = (
    spec->parameters := cons(
       '{ short_name = Some(arg.short_name)
        , long_name = arg.long_name
        , description = arg.description
        , hint = Some(arg.hint)
        , has_argument = true
        }, spec->parameters);
    let val inserted = hashtbl_insert(spec->short_to_long, arg.short_name, arg.long_name)
    in
        option_vt_free(inserted);
    end
)

implement arg_parser_add_flag_long(parser, flag) =
    command_spec_add_flag_long(parser.default_command_spec, flag)

implement command_spec_add_flag_long(spec, flag) =
    spec->parameters := cons(
       '{ short_name = None()
        , long_name = flag.long_name
        , description = flag.description
        , hint = None()
        , has_argument = false
        }, spec->parameters)

implement arg_parser_add_flag_short_long(parser, flag) =
    command_spec_add_flag_short_long(parser.default_command_spec, flag)

implement command_spec_add_flag_short_long(spec, flag) = (
    spec->parameters := cons(
       '{ short_name = Some(flag.short_name)
        , long_name = flag.long_name
        , description = flag.description
        , hint = None()
        , has_argument = false
        }, spec->parameters);
    let val inserted = hashtbl_insert(spec->short_to_long, flag.short_name, flag.long_name)
    in
        option_vt_free(inserted);
    end
)

fun parser_insert_name(parser: Parser, name: String, value: Option(String)) =
    let val long_name =
            case+ hashtbl_search(parser.spec->short_to_long, name) of
            | ~Some_vt(long_name) => long_name
            | ~None_vt() => name
        val inserted = hashtbl_insert(parser.name_values, long_name, value)
    in
        option_vt_free(inserted)
    end

overload .insert_name with parser_insert_name

fun parser_value(parser: Parser, param: Param): Result(Option(String), Error) =
    if ~param.has_argument then (* TODO: use dependent type to tie this field to the returned option? *)
        Ok(None())
    else
        case+ !(parser.args) of
        | list_nil() => Err(MissingValue(param.long_name))
        | list_cons(arg, new_args) => (
            !(parser.args) := new_args;
            Ok(Some(arg))
        )

overload .value with parser_value

fun parser_find_param(parser: Parser, name: String): Result(Param, Error) =
    let implement list_find$pred<Param>(param) = param.long_name = name
        val param = list_find_opt(parser.spec->parameters)
    in
        case+ param of
        | ~Some_vt(param) => Ok(param)
        | ~None_vt() => Err(ParamDoesNotExist(name))
    end

overload .find_param with parser_find_param

fun string_substring {n:int}{st,ln:nat | st+ln <= n} (str: string(n), start: size_t st, length: size_t ln): String =
    strnptr2string(string_make_substring(str, start, length))

fun parser_long_arg(parser: Parser, arg: stringGt(2)): Result((), Error) =
    let val name = string_substring(arg, i2sz(2), string_length(arg) - i2sz(2))
    in
        result_and_then(parser.find_param(name), lam(param) =>
            result_map(parser.value(param), lam(value) => (
                parser.insert_name(name, value);
                @()
            ))
        )
    end

overload .long_arg with parser_long_arg

fun parser_short_args{n:nat | n >= 2}(parser: Parser, arg: string(n)): Result((), Error) =
    let val arg_len = string_length(arg)
        fun foreach(index: Size_t): Result((), Error) =
            if index < arg_len then
                let val letter = g1ofg0(char2string(arg[index]))
                    val long_name =
                        case+ hashtbl_search(parser.spec->short_to_long, letter) of
                        | ~Some_vt(long_name) => long_name
                        | ~None_vt() => letter
                in
                    result_and_then(parser.find_param(long_name), lam(param) => (
                        if param.has_argument then
                            (* + 1 to go to after the current argument. *)
                            let val start = index + 1
                                val rest = string_substring(arg, start, string_length(arg) - start)
                            in
                                (* If the parameter has an argument, it is the rest of the string. *)
                                if string_isnot_empty(rest) then
                                    !(parser.args) := cons(rest, !(parser.args));
                            end;
                        result_and_then(parser.value(param), lam(value) =>
                            let val inserted = hashtbl_insert(parser.name_values, long_name, value)
                            in (
                                option_vt_free(inserted);
                                if param.has_argument then
                                    (* If the parameter requires an argument, we stop here, because there cannot be
                                     * other short parameters after a parameter with an arguement. *)
                                    Ok(@())
                                else
                                    (* If there are no arguments for the current parameter, we can continue processing
                                     * the short parameters. *)
                                    foreach(index + 1)
                            )
                            end
                        )
                    ))
                end
            else
                Ok(())
    in
        foreach(i2sz(1)) (* Start at 1 to skip the initial dash. *)
    end

overload .short_args with parser_short_args

fun string_is_prefix {m, n: nat} (string_prefix: string(m), string: string(n)): bool =
    let val prefix_len = string_length(string_prefix)
        val string_len = string_length(string)
        fun is_prefix {i: nat | m <= n; i < m} (index: size_t(i)): bool =
            let val equal = string_get_at(string, index) = string_get_at(string_prefix, index)
            in
                if index + 1 >= prefix_len then
                    equal
                else
                    equal && is_prefix(index + 1)
            end
    in
        if prefix_len > string_len then
            false
        else if prefix_len > 0 then
            is_prefix(i2sz(0))
        else
            true
    end

fun parse_matches(parser: Parser, subcommand: Option(String)): Result(Matches, Error) =
    let fun fetch_name_values(): Result((), Error) =
            case+ !(parser.args) of
            | list_nil() => Ok(())
            | list_cons(arg, args) => (
                !(parser.args) := args;
                if string_is_prefix("-", arg) then
                    if string_length(arg) > 0 then
                        if string_is_prefix("--", arg) then
                            if string_length(arg) > 2 then
                                result_and_then(parser.long_arg(arg), lam(@()) => fetch_name_values())
                            else (
                                (* FIXME: the rest are free args too. *)
                                !(parser.free_args) := cons(arg, !(parser.free_args));
                                fetch_name_values()
                            )
                        else if string_length(arg) > 1 then
                            result_and_then(parser.short_args(arg), lam(@()) => fetch_name_values())
                        else (
                            !(parser.free_args) := cons(arg, !(parser.free_args));
                            fetch_name_values()
                        )
                    else
                        $raise Unreachable (* TODO: make string_is_prefix() prove that arg has the length of the prefix at the minimum. *)
                else (
                    !(parser.free_args) := cons(arg, !(parser.free_args));
                    fetch_name_values()
                )
            )
    in (
        case+ fetch_name_values() of
        | Ok(@()) =>
            Ok('{ args = parser.name_values
                , free_args = list_of_list_vt(list_reverse(!(parser.free_args)))
                , short_to_long = parser.spec->short_to_long
                , subcommand = subcommand
                })
        | Err(error) => Err(error)
    )
    end

overload .matches with parse_matches

fun parse_subcommands(parser: Parser): Result(Matches, Error) =
    case+ !(parser.args) of
    | list_nil() => parser.matches(None())
    | list_cons(arg, args) =>
        case+ hashtbl_search(parser.subcommands, arg) of
        | ~Some_vt(subcommand) => (
            !(parser.args) := args;
            !(parser.spec) := !subcommand;
            parser.matches(Some(arg))
        )
        | ~None_vt() => parser.matches(None())

overload .subcommands with parse_subcommands

implement arg_parser_parse(parser) =
    let val free_args: List0(String) = nil()
        val args: List0(String) = parser.args
        val parser: Parser =
           '{ args = ref(args)
            , free_args = ref(free_args)
            , name_values = hashtbl_make_nil<String, Option(String)>(i2sz(10))
            , spec = ref(
               @{ parameters = parser.default_command_spec->parameters
                , short_to_long = parser.default_command_spec->short_to_long
                , description = parser.default_command_spec->description
                }
            )
            , subcommands = parser.subcommands
            }
    in
        parser.subcommands()
    end

fun {a: t@ype} option_flatten(optional: Option(Option(a))): Option(a) =
  case+ optional of
  | None() => None()
  | Some(value) => value

(* TODO: add check to see if called on a param that takes a string value *)
(* TODO: add check to see if the argument was added with add_arg instead of add_flag. *)
implement matches_get_string(matches, name) =
    let val long_name =
            case+ hashtbl_search(matches.short_to_long, name) of
            | ~Some_vt(long_name) => long_name
            | ~None_vt() => name
    in
        option_flatten(option_of_option_vt(hashtbl_search(matches.args, long_name)))
    end

implement matches_get_flag(matches, name) =
    let val long_name =
            case+ hashtbl_search(matches.short_to_long, name) of
            | ~Some_vt(long_name) => long_name
            | ~None_vt() => name
    in
        option_is_some(option_of_option_vt(hashtbl_search(matches.args, long_name)))
    end

implement matches_get_free_args(matches) =
    matches.free_args

implement matches_is_subcommand(matches, name) =
    case+ matches.subcommand of
    | Some(subcommand) => subcommand = name
    | None() => false

implement arg_parser_add_subcommand(parser, name, description) =
    let val subcommand = ref(
           @{ parameters = nil()
            , short_to_long = hashtbl_make_nil<String, String>(i2sz(10))
            , description = description
            }
        )
        val inserted = hashtbl_insert(parser.subcommands, name, subcommand)
    in
        option_vt_free(inserted);
        subcommand
    end

implement arg_parser_set_description(parser, description) =
    !(parser.description) := description

implement arg_parser_set_program_name(parser, name) =
    !(parser.program_name) := name

fun string_append {n1,n2:int} (x1: string(n1), x2: string(n2)): string(n1 + n2) =
    let val string = string1_append(x1, x2)
    in
        if strnptr_length(string) >= 0 then
            strnptr2string(string)
        else (
            free(string);
            $raise Unreachable
        )
    end

overload + with string_append

fun string_make{n:nat}(length: int(n), char: charNZ): string(n) =
    strnptr2string(string_make_list_vt(list_make_elt(length, char)))

fun format_hint(hint: Option(String0)): String =
    case+ hint of
    | Some(hint) => " <" + hint + ">"
    | None() => ""

fun param_string{n:nat}(param: Param, length: size_t(n)): String =
    let val short_name =
            case+ param.short_name of
            | Some(name) => "-" + name + ", "
            | None() => "    "
        val hint = format_hint(param.hint)
        val hint_length = sz2i(string_length(hint))
        val param_len = sz2i(string_length(param.long_name))
        val num_spaces = sz2i(minimum_spaces + length) - param_len
        val num_spaces = num_spaces - hint_length
        val spaces =
            if num_spaces >= 0 then
                string_make(num_spaces, ' ')
            else
                $raise Unreachable (* FIXME: prove that num_spaces >= 0 instead of checking it at runtime. *)
    in
        "    " + short_name + "--" + param.long_name + hint + spaces + param.description + "\n"
    end

fun command_spec_string(spec: CommandSpec): String =
    let val max_length = list_foldleft<Param, Size_t>(spec.parameters, i2sz(0), lam(maximum, param) =>
            let val hint_length =
                (case+ param.hint of
                | Some(hint) => string_length(hint) + hint_margin_length
                | None() => i2sz(0)): Size_t
            in
                max(maximum, gadd_val_val<Size_t>(string_length(param.long_name), hint_length))
            end
        )
    in
        list_foldleft<Param, String>(spec.parameters, "", lam(result, param) =>
            result + param_string(param, max_length)
        )
    end

fun spec_usage_string(parser: ArgParser, spec: CommandSpec, subcommand: Option(String)): String =
    let val subcommand_string =
            (case+ subcommand of
            | Some(subcommand) => " " + subcommand
            | None() => ""): String
        val show_subcommands = hashtbl_get_size(parser.subcommands) > 0 && option_is_none(subcommand)
        val subcommand_hint =
            (if show_subcommands then
                " [SUBCOMMAND]\n"
            else
                "\n"): String
        val result = ref<String>(
            !(parser.description) + "\n\n" + "USAGE:\n" + "    " + !(parser.program_name) + subcommand_string +
                " [OPTIONS]" + subcommand_hint + "\nOPTIONS:\n" + command_spec_string(spec)
        )
    in (
        if show_subcommands then
            let val max_length = ref<Size_t>(i2sz(0))
            in (
                hashtbl_foreach_cloref(parser.subcommands, lam(name, spec) =>
                    !max_length := max(!max_length, string_length(name))
                );
                !result := !result + "\nSUBCOMMANDS:\n";
                hashtbl_foreach_cloref(parser.subcommands, lam(name, spec) =>
                    let val num_spaces = sz2i(minimum_spaces) + sz2i(!max_length) - sz2i(string_length(name))
                        val spaces =
                            if num_spaces >= 0 then
                                string_make(num_spaces, ' ')
                            else
                                $raise Unreachable (* FIXME: prove that num_spaces >= 0 instead of checking it at runtime. *)
                    in
                        !result := !result + "    " + name + spaces + !spec.description + "\n"
                    end
                )
            )
            end;
        !result
    )
    end

implement arg_parser_print_usage(parser) =
    print!(arg_parser_usage_string(parser))

implement arg_parser_usage_string(parser) =
    spec_usage_string(parser, !(parser.default_command_spec), None())

implement arg_parser_print_subcommand_usage(parser, subcommand) =
    result_map(arg_parser_subcommand_usage_string(parser, subcommand), lam(usage) => (
        print!(usage);
        @()
    ))

implement arg_parser_subcommand_usage_string(parser, subcommand) =
    case+ hashtbl_search(parser.subcommands, subcommand) of
    | ~Some_vt(spec) => Ok(spec_usage_string(parser, !spec, Some(subcommand)))
    | ~None_vt() => Err(UnknownSubcommand(subcommand))

implement error_string(error) =
    case+ error of
    | MissingValue(name) => "Missing value for parameter " + name
    | ParamDoesNotExist(param) => "No parameter named " + param
    | UnknownSubcommand(subcommand) => "No subcommand named " + subcommand

implement prerr_error(error) =
    prerr!(error_string(error))
