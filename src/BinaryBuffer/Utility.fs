namespace BinaryBuffer

module rec Utility =
    open System
    
    let internal toBitConverterTuple byteSeq = byteSeq |> Array.ofSeq, 0
        
    let toUInt16 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToUInt16
    let toUInt32 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToUInt32
    let toUInt64 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToUInt64
    
    let toInt16 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToInt16
    let toInt32 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToInt32
    let toInt64 (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToInt64
    
    let toSingle (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToSingle
    let toDouble (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToDouble
    let toChar (bytes: uint8 seq) = bytes |> toBitConverterTuple |> BitConverter.ToChar
    
    let toBoolean (byte: uint8) = [ byte ] |> toBitConverterTuple |> BitConverter.ToBoolean
    
    let getBytesFromUInt16 (value: uint16) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromUInt32 (value: uint32) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromUInt64 (value: uint64) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromInt16 (value: int16) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromInt32 (value: int32) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromInt64 (value: int64) = BitConverter.GetBytes value |> List.ofArray
    
    let getBytesFromSingle (value: single) = BitConverter.GetBytes value |> List.ofArray
    let getBytesFromDouble (value: double) = BitConverter.GetBytes value |> List.ofArray

module rec Encoding =
    open System.Text
    
    let getString (encoding: Encoding) (bytes: byte array) = encoding.GetString bytes
    let getBytes (encoding: Encoding) (string: string) = encoding.GetBytes string