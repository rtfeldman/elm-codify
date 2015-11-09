module Codify.PrettyPrint (typeAlias) where

import Codify exposing (Type(..))

import String
import Dict exposing (Dict)


typeAlias : String -> Int -> Type -> String
typeAlias str indentSpaces typeValue =
    "type alias " ++ str ++ " =\n" ++ (typeAliasHelp 1 indentSpaces typeValue)

typeAliasHelp : Int -> Int -> Type -> String
typeAliasHelp indentLevel indentSpaces typeValue =
    let
        indentStr =
            String.repeat (indentSpaces * indentLevel) " "

        subIndentStr =
            indentStr ++ String.repeat (indentSpaces) " "

        str =
            case typeValue of
                IntType intVal ->
                    toString intVal
                FloatType floatVal ->
                    toString floatVal
                StringType str ->
                    toString str
                ListType list ->
                    if List.isEmpty list then
                        "[]"
                    else
                        let
                            listBody =
                                list
                                    |> List.map (typeAliasHelp 0 indentSpaces)
                                    |> String.join ("\n" ++ indentStr ++ ", ")
                        in
                            "[ " ++ listBody ++ "\n" ++ indentStr ++ "]"
                RecordType name dict ->
                    if Dict.isEmpty dict then
                        "{}"
                    else
                        let
                            printTypedField (field, fieldType) =
                                field
                                    ++ " : "
                                    ++ (typeAliasHelp 0 indentSpaces fieldType)

                            --type alias Foo =
                            --    { bar : 12.3
                            --    , baz : \"baz\"
                            --    , foo : 1
                            --bar : 12.3
                            --    , baz : \"baz\"
                            --    , foo : 1}


                            dictBody =
                                Debug.log "DICT IS" dict
                                    |> Dict.toList
                                    |> List.map printTypedField
                                    |> String.join ("\n" ++ indentStr ++ ", ")
                        in
                            "{ " ++ dictBody ++ "\n" ++ dictBody ++ "}"
                NullType ->
                    "null"
                UndefinedType ->
                    "undefined"
    in
        indentStr ++ str

