import dfhdl._
import scala.math._

//* Simple Parametrizable Memory */
class SimpleMem(val width : Int = 32, val depth : Int = 4096) extends RTDesign:
    val addr = Bits.until(depth) <> IN
    val en = Bit <> IN
    val data_in = Bits(width) <> IN
    val data_out = Bits(width) <> OUT.REG
    
    val data = Bits(width).X(depth) <> VAR.REG init all(all(0))
    
    if (en)
        data(addr).din := data_in
    else
        data_out.din := data(addr)
        
        
        
class LZWSimpleMemIF() extends RTDesign:
    val entries_from_mem = Bits(20) <> IN
    val entries = Bits(20) <> OUT
    val en = Bit <> IN
    val en_to_mem = Bit <> OUT
    val addr = Bits(12) <> IN
    val addr_to_mem = Bits(12) <> OUT
    val entries_valid = Bit <> OUT.REG
    val new_entry = Bits(20) <> IN
    val new_entry_to_mem = Bits(20) <> OUT
    val rd = Bit <> IN
    
    if (en)
        en_to_mem := 1
        new_entry_to_mem := new_entry
    else
        en_to_mem := 0
        new_entry_to_mem := all(0)
        
    addr_to_mem := addr
    
    if (rd)
        entries_valid.din := 1
    else
        entries_valid.din := 0
    
    if (entries_valid)
        entries := entries_from_mem
    else
        entries := all(0)
    
    
