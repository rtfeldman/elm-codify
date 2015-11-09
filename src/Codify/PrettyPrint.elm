module Codify.PrettyPrint (typeToString) where

import Codify exposing (Type(..))

--import String
--import Dict exposing (Dict)


--typeAlias : String -> Int -> Type -> String
--typeAlias str indentSpaces typeValue =
--    "type alias " ++ str ++ " =\n" ++ (typeAliasHelp 1 indentSpaces typeValue)

--typeAliasHelp : Int -> Int -> Type -> String
--typeAliasHelp indentLevel indentSpaces typeValue =
--    let
--        indentStr =
--            String.repeat (indentSpaces * indentLevel) " "

--        subIndentStr =
--            indentStr ++ String.repeat (indentSpaces) " "

--        str =
--            case typeValue of
--                IntType ->
--                    toString intVal

--                FloatType ->
--                    toString floatVal

--                StringType ->
--                    toString str

--                TupleType list ->
--                    if List.isEmpty list then
--                        "[]"
--                    else
--                        let
--                            listBody =
--                                list
--                                    |> List.map (typeAliasHelp 0 indentSpaces)
--                                    |> String.join ("\n" ++ indentStr ++ ", ")
--                        in
--                            "[ " ++ listBody ++ "\n" ++ indentStr ++ "]"

--                RecordType dict ->
--                    if Dict.isEmpty dict then
--                        "{}"
--                    else
--                        let
--                            printTypedField (field, fieldType) =
--                                field
--                                    ++ " : "
--                                    ++ (typeAliasHelp 0 indentSpaces fieldType)

--                            --type alias Foo =
--                            --    { bar : 12.3
--                            --    , baz : \"baz\"
--                            --    , foo : 1
--                            --bar : 12.3
--                            --    , baz : \"baz\"
--                            --    , foo : 1}


--                            dictBody =
--                                Debug.log "DICT IS" dict
--                                    |> Dict.toList
--                                    |> List.map printTypedField
--                                    |> String.join ("\n" ++ indentStr ++ ", ")
--                        in
--                            "{ " ++ dictBody ++ "\n" ++ dictBody ++ "}"
--                NullType ->
--                    "null"
--                UndefinedType ->
--                    "undefined"
--    in
--        indentStr ++ str


typeToString : Bool -> Type -> String
typeToString needsParens typeValue =
    let
        (supportsParens, baseResult) =
            case typeValue of
                AliasType aliasName ->
                    (False, aliasName)

                IntType ->
                    (False, "Int")

                FloatType ->
                    (False, "Float")

                StringType ->
                    (False, "String")

                ListType typeParam ->
                    (True, "List " ++ (typeToString True typeParam))

                MaybeType typeParam ->
                    (True, "Maybe " ++ (typeToString True typeParam))

                RecordType typeDict ->
                    (True, "{ TODO }")

                TupleType typeParams ->
                    (True, "( TODO )")
    in
        if needsParens && supportsParens then
            "(" ++ baseResult ++ ")"
        else
            baseResult
