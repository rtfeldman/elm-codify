module Codify.PrettyPrint (typeAlias, typeToString) where

import Codify exposing (Type(..))

import String
import Dict exposing (Dict)


typeAlias : String -> Type -> String
typeAlias str typeValue =
    "type alias " ++ str ++ " =\n" ++ (typeToString False typeValue)


fieldPairToString : (String, Type) -> String
fieldPairToString (field, fieldType) =
    field ++ " :" ++ (nestedTypeToString fieldType)


nestedTypeToString : Type -> String
nestedTypeToString nestedType =
    case nestedType of
        RecordType _ ->
            "\n" ++ (indent (typeToString False nestedType))

        TupleType _ ->
            "\n" ++ (indent (typeToString False nestedType))

        _ ->
            " " ++ (typeToString False nestedType)


indentSpaces : Int
indentSpaces =
    4


indentation : String
indentation =
    String.repeat indentSpaces " "


indent : String -> String
indent str =
    str
        |> String.split "\n"
        |> List.map (\line -> indentation ++ line)
        |> String.join "\n"


multilineType : String -> String -> String -> List String -> String
multilineType openingDelimiter closingDelimiter elemDelimiter lines =
    let
        header =
            indentation ++ openingDelimiter ++ " "

        body =
            String.join ("\n" ++ indentation ++ elemDelimiter ++ " ") lines

        footer =
            "\n" ++ indentation ++ closingDelimiter
    in
        header ++ body ++ footer


typeToString : Bool -> Type -> String
typeToString needsParens typeValue =
    let
        ( supportsParens, baseResult ) =
            case typeValue of
                AliasType aliasName ->
                    ( False, aliasName )

                IntType ->
                    ( False, "Int" )

                FloatType ->
                    ( False, "Float" )

                StringType ->
                    ( False, "String" )

                ListType typeParam ->
                    ( True, "List " ++ (typeToString True typeParam) )

                MaybeType typeParam ->
                    ( True, "Maybe " ++ (typeToString True typeParam) )

                RecordType typeDict ->
                    if Dict.isEmpty typeDict then
                        ( False, "{}" )
                    else
                        let
                            str =
                                typeDict
                                    |> Dict.toList
                                    |> List.map fieldPairToString
                                    |> multilineType "{" "}" ","
                        in
                            ( True, str )

                TupleType typeParams ->
                    if List.isEmpty typeParams then
                        ( False, "()" )
                    else
                        let
                            str =
                                typeParams
                                    |> List.map nestedTypeToString
                                    |> multilineType "(" ")" ","
                        in
                            ( True, str )
    in
        if needsParens && supportsParens then
            "(" ++ baseResult ++ ")"
        else
            baseResult
