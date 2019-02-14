namespace BinaryBuffer

module rec Main = 
  open System.Text
  open Utility
  
  type internal Buf = uint8 list
  type internal Result<'T> = 'T * Buf
  
  let create = List.empty
  
  let fromArray arr = arr |> List.ofArray
  let toArray buf = Array.ofList buf
  
  let append (buf: Buf) (buf2: Buf) = buf @ buf2
  
  let appendSegmentsTo (buf: Buf) (segments: Buf seq) =
    Seq.append (seq [ buf ]) segments |> Seq.fold (fun st1 st2 -> append st1 st2) []
  
  let writeByte (value: uint8) (buffer: Buf) = buffer @ [ value ]
  
  let writeByteList (value: uint8 list) (buffer: Buf) = buffer @ value
  let writeByteSeq seq buffer = writeByteList (seq |> List.ofSeq) buffer
  
  let writeUInt8 (value: uint8) (buffer: Buf) = buffer @ [ value ]
  let writeUInt8List (value: uint8 list) (buffer: Buf) = buffer @ value
  
  let writeUInt16 (value: uint16) buffer = value |> getBytesFromUInt16 |> writeUInt8List <| buffer
  let writeUInt32 (value: uint32) buffer = value |> getBytesFromUInt32 |> writeUInt8List <| buffer
  let writeUInt64 (value: uint64) buffer = value |> getBytesFromUInt64 |> writeUInt8List <| buffer
  let writeInt8 (value: int8) buffer = buffer |> writeUInt8 (uint8 value)
  let writeInt16 (value: int16) buffer = value |> getBytesFromInt16 |> writeUInt8List <| buffer
  let writeInt32 (value: int32) buffer = value |> getBytesFromInt32 |> writeUInt8List <| buffer
  let writeInt64 (value: int64) buffer = value |> getBytesFromInt64 |> writeUInt8List <| buffer
  
  let writeBoolean (value: bool) buffer = buffer |> writeUInt8 (if value then 1uy else 0uy)
    
  let writeSingle (value: single) buffer = value |> getBytesFromSingle |> writeUInt8List <| buffer
  let writeDouble (value: double) buffer = value |> getBytesFromDouble |> writeUInt8List <| buffer
  let writeString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeInt32 (value |> String.length) |> writeByteSeq (value |> Encoding.getBytes encoding)
  let writeCString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeByteSeq (value |> Encoding.getBytes encoding) |> writeUInt8 0uy
  
  let next fn res = fst res |> fn, snd res
  
  let readByte (buf: Buf) = buf.[0], buf |> List.tail
    
  let readByteList count buf = buf |> List.take count, buf |> List.skip count
  
  let readUInt8  (buf: Buf) = readByte buf |> next uint8
  let readUInt16 (buf: Buf) = readByteList 2 buf |> next toUInt16
  let readUInt32 (buf: Buf) = readByteList 4 buf |> next toUInt32
  let readUInt64 (buf: Buf) = readByteList 8 buf |> next toUInt64
  let readInt8   (buf: Buf) = readByte buf |> next int8
  let readInt16  (buf: Buf) = readByteList 2 buf |> next toInt16
  let readInt32  (buf: Buf) = readByteList 4 buf |> next toInt32
  let readInt64  (buf: Buf) = readByteList 8 buf |> next toInt64
  
  let readUInt8List count (buf: Buf) = readByteList count buf
  
  let readSingle (buf: Buf) = readUInt8List 4 buf |> next toSingle
  let readDouble (buf: Buf) = readUInt8List 8 buf |> next toDouble
  let readBoolean (buf: Buf) = readUInt8 buf |> next (fun byte -> if byte = 0uy then false else true)
  
  let readString (encoding: Encoding) (buf: Buf) =
    buf
    |> readInt32
    |> fun (length, buf) -> readUInt8List length buf
    |> next (fun bytes -> bytes |> Array.ofList |> Encoding.getString encoding)
      
  let readCString (encoding: Encoding) (buf: Buf) =
    Seq.findIndex (fun byte -> byte = 0uy) (seq buf)
    |> fun idx -> Seq.take idx buf
    |> Array.ofSeq
    |> fun bytes -> Encoding.getString encoding bytes