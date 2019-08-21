namespace BinaryBuffer

open System
open Microsoft.FSharpLu

[<RequireQualifiedAccess>]
module rec Binary = 
  open System.Text
  open Utility
  
  type Buf = {
    buffer: uint8 list
  }
  
  type Buffer = Buf
  
  let isEmpty buf = buf.buffer |> Seq.isEmpty
  let length buf = buf.buffer |> Seq.length
  let getBuffer buf = buf.buffer
  
  type internal Result<'T> = 'T * Buf

  let isLittleEndian = BitConverter.IsLittleEndian
  let toBE bytes = if isLittleEndian then bytes |> List.rev else bytes
  let toLE bytes = if isLittleEndian then bytes else bytes |> List.rev

  let create = { buffer = [] }

  let fromList lst = { buffer = lst }
  let fromArray arr = arr |> List.ofArray |> fromList
  
  let toArray buf = Array.ofList (buf |> getBuffer)
  let toSeq buf = buf |> getBuffer |> Seq.ofList
  let toList buf = buf |> getBuffer
  
  let next fn res = fst res |> fn, snd res
  let map fn (v, buf) = (fn v), buf
  
  let skip count (buf: Buf): Buf = { buf with buffer = buf.buffer |> Seq.skip count |> List.ofSeq }
  let take count (buf: Buf): Buf = { buf with buffer = buf.buffer |> Seq.take count |> List.ofSeq }
  
  let replace ``begin`` length (chunk: Buf) (destination: Buf) =
    let buf1 = destination |> take ``begin``
    let buf2 = destination |> skip (``begin`` + length)
    
    append buf1 chunk |> append <| buf2
  
  let append (buf: Buf) (buf2: Buf) =
    { buf with buffer = buf.buffer @ buf2.buffer }
    
  let prepend (buf: Buf) (buf2: Buf) =
    { buf with buffer = buf2.buffer @ buf.buffer }
  
  let appendSegmentsTo (buf: Buf) (segments: Buf seq) =
    Seq.append (seq [ buf ]) segments |> Seq.fold (fun st1 st2 -> append st1 st2) create 
  
  let writeByte (value: uint8) (buffer: Buf) = { buffer with buffer = buffer.buffer @ [ value ] }
  let writeChar (value: char) (buffer: Buf) = writeByte (uint8 value) buffer
  
  let writeByteList (value: uint8 list) (buffer: Buf) = { buffer with buffer = buffer.buffer @ value }
  let writeByteSeq seq buffer = seq |> List.ofSeq |> writeByteList <| buffer
  
  let writeUInt8 = writeByte
  let writeUInt8List = writeByteList
  let writeUInt8Array = List.ofArray >> writeUInt8List
  
  let internal writeSegment value buffer =
    value |> writeUInt8List <| buffer
  
  let writeUInt16 (value: uint16) buffer = value |> getBytesFromUInt16 |> writeSegment <| buffer 
  let writeUInt32 (value: uint32) buffer = value |> getBytesFromUInt32 |> writeSegment <| buffer
  let writeUInt64 (value: uint64) buffer = value |> getBytesFromUInt64 |> writeSegment <| buffer
  let writeInt8 (value: int8) buffer = buffer |> writeUInt8 (uint8 value)
  let writeInt16 (value: int16) buffer = value |> getBytesFromInt16 |> writeSegment <| buffer
  let writeInt32 (value: int32) buffer = value |> getBytesFromInt32 |> writeSegment <| buffer
  let writeInt64 (value: int64) buffer = value |> getBytesFromInt64 |> writeSegment <| buffer
  
  let writeUInt16BE (value: uint16) buffer = value |> getBytesFromUInt16 |> toBE |> writeSegment <| buffer 
  let writeUInt32BE (value: uint32) buffer = value |> getBytesFromUInt32 |> toBE |> writeSegment <| buffer
  let writeUInt64BE (value: uint64) buffer = value |> getBytesFromUInt64 |> toBE |> writeSegment <| buffer
  let writeInt16BE (value: int16) buffer = value |> getBytesFromInt16 |> toBE |> writeSegment <| buffer
  let writeInt32BE (value: int32) buffer = value |> getBytesFromInt32 |> toBE |> writeSegment <| buffer
  let writeInt64BE (value: int64) buffer = value |> getBytesFromInt64 |> toBE |> writeSegment <| buffer
  
  let writeBoolean (value: bool) buffer = buffer |> writeUInt8 (if value then 1uy else 0uy)
    
  let writeSingle (value: single) buffer = value |> getBytesFromSingle |> writeSegment <| buffer
  let writeDouble (value: double) buffer = value |> getBytesFromDouble |> writeSegment <| buffer
  let writeChars (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeUInt8List (encoding.GetBytes value |> List.ofArray)
    
  let writeString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeInt32 (value |> String.length) |> writeByteSeq (value |> Encoding.getBytes encoding)
  let writeCString (value: string) (encoding: Encoding) (buffer: Buf) =
    buffer |> writeByteSeq (value |> Encoding.getBytes encoding) |> writeUInt8 0uy
  
  let readByte (buf: Buf) = buf.buffer |> List.head, { buf with buffer = buf.buffer |> List.tail }
  let readChar (buf: Buf) = buf |> readByte |> next char
  
  let readByteList count buf = buf.buffer |> List.take count, buf |> skip count
  let readBuffer count buf = buf |> take count, buf |> skip count
  
  let readUInt8  (buf: Buf) = readByte buf |> next uint8
  let readUInt16 (buf: Buf) = readByteList 2 buf |> fun (bytes, buf) -> (bytes, buf) |> next toUInt16
  let readUInt32 (buf: Buf) = readByteList 4 buf |> fun (bytes, buf) -> (bytes, buf) |> next toUInt32
  let readUInt64 (buf: Buf) = readByteList 8 buf |> fun (bytes, buf) -> (bytes, buf) |> next toUInt64
  let readInt8   (buf: Buf) = readByte buf |> next int8
  let readInt16  (buf: Buf) = readByteList 2 buf |> fun (bytes, buf) -> (bytes, buf) |> next toInt16
  let readInt32  (buf: Buf) = readByteList 4 buf |> fun (bytes, buf) -> (bytes, buf) |> next toInt32
  let readInt64  (buf: Buf) = readByteList 8 buf |> fun (bytes, buf) -> (bytes, buf) |> next toInt64
  
  let readUInt16BE (buf: Buf) = readByteList 2 buf |> map (toBE >> toUInt16) 
  let readUInt32BE (buf: Buf) = readByteList 4 buf |> map (toBE >> toUInt32) 
  let readUInt64BE (buf: Buf) = readByteList 8 buf |> map (toBE >> toUInt64) 
  let readInt16BE  (buf: Buf) = readByteList 2 buf |> map (toBE >> toInt16) 
  let readInt32BE  (buf: Buf) = readByteList 4 buf |> map (toBE >> toInt32) 
  let readInt64BE  (buf: Buf) = readByteList 8 buf |> map (toBE >> toInt64) 
  
  let readUInt8List count (buf: Buf) = readByteList count buf
  
  let readSingle (buf: Buf) = readUInt8List 4 buf |> fun (bytes, buf) -> (bytes, buf) |> next toSingle
  let readDouble (buf: Buf) = readUInt8List 8 buf |> fun (bytes, buf) -> (bytes, buf) |> next toDouble
  let readBoolean (buf: Buf) = readUInt8 buf |> next (fun byte -> byte = 0uy |> not)
  
  let readString (encoding: Encoding) (buf: Buf) =
    buf
    |> readInt32
    |> fun (length, buf) -> readUInt8List length buf
    |> next (fun bytes -> bytes |> Array.ofList |> Encoding.getString encoding)
      
  let readCString (encoding: Encoding) (buf: Buf) =
    Seq.findIndex ((=) 0uy) buf.buffer
    |> fun idx -> buf |> take idx |> toArray |> Encoding.getString encoding, buf |> skip (1 + idx)
    
  let stringifyList (buffer: uint8 list) =
    buffer |> Seq.map(fun b -> b.ToString "x2") |> Text.join " "
    
  let stringify (buffer: Buf) = buffer |> getBuffer |> stringifyList
  