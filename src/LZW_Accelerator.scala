import dfhdl.*
import scala.math.*

val clkCfg = ClkCfg(ClkCfg.Edge.Rising)
val rstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.Low)
val RTcfg = RTDomainCfg(clkCfg, rstCfg)


// Interfaces
case class LZWControlDictIn (
    idx_from_dp : Bits[12] <> VAL,
    done : Bit <> VAL,
    found : Bit <> VAL
) extends Struct

case class LZWControlDictOut (
    idx_to_dp : Bits[12] <> VAL,
    symbol : Bits[8] <> VAL,
    start : Bit <> VAL
) extends Struct

case class LZWControlDebug (
    a : Bit <> VAL
) extends Struct


// Controller
class LZWControl(val debug : Boolean = true) extends RTDesign():
    import LZWCtrlState.*
    // Data In
    val ready_in = Bit <> OUT
    val valid_in = Bit <> IN
    val data_in = Bits(8) <> IN
    
    // Dict Control to Datapath IF
    val dict_in = LZWControlDictIn <> IN
    val dict_out = LZWControlDictOut <> OUT
    
    // CSR
    val control = Bits(32) <> IN
    val status = Bits(32) <> OUT
    val finish = Bit <> IN
    val clear_finish = Bit <> OUT
    
    // Data Out
    val valid_out = Bit <> OUT
    val ready_out = Bit <> IN
    val data_out = Bits(12) <> OUT

    // Add Debug
            
    // Registers
    val state = LZWCtrlState <> VAR.REG init Idle
    val continued = Bit <> VAR.REG init 0
    val idx_r = Bits(12) <> VAR.REG init h"000"
    val sym_r = Bits(8) <> VAR.REG init h"00"
    val locked = Bit <> VAR.REG init 0
    

    status := all(0)
    clear_finish := 0
    
    ready_in := 0
    dict_out.start := 0
    valid_out := 0
    data_out := h"000"
    state match {
        case Idle => 
            if (valid_in && !locked)
                ready_in := 1
                state.din := Receive

                dict_out.start := 1                
                if (!continued)
                    dict_out.idx_to_dp := h"FFF"
                    dict_out.symbol := data_in
                else
                    dict_out.idx_to_dp := idx_r
                    dict_out.symbol := data_in
                    sym_r.din := data_in
            else
                if (finish)
                    locked.din := 1
                    if (continued)
                        valid_out := 1
                        data_out := idx_r
                        if (ready_out)
                            clear_finish := 1
                            continued.din := 0
                            locked.din := 0
                    else
                        clear_finish := 1
                        locked.din := 0

                
        case Receive => 
            if (dict_in.done)
                if (!dict_in.found || (dict_in.found && dict_in.idx_from_dp == h"FFF"))
                    continued.din := 0
                    valid_out := 1
                    
                    if (dict_in.found)
                        data_out := dict_in.idx_from_dp
                    else
                        data_out := idx_r
                        
                    if (ready_out)
                        if (!dict_in.found)
                            dict_out.start := 1
                            dict_out.idx_to_dp := h"FFF"
                            dict_out.symbol := sym_r
                            state.din := Receive
                        else
                            if (!valid_in)
                                state.din := Idle
                            else
                                ready_in := 1
                                state.din := Receive
                                
                                dict_out.start := 1
                                dict_out.idx_to_dp := h"FFF"
                                dict_out.symbol := data_in
                    
                else
                    if (!continued)
                        idx_r.din := (b"0000", dict_in.idx_from_dp(7, 0))
                    else
                        idx_r.din := dict_in.idx_from_dp
                    continued.din := 1
                    if (!valid_in)
                        state.din := Idle
                    else
                        // found and immediately continue
                        ready_in := 1
                        state.din := Receive
                        sym_r.din := data_in
                        
                        dict_out.start := 1
                        if (!continued)
                            dict_out.idx_to_dp := (b"0000", dict_in.idx_from_dp(7, 0))
                        else
                            dict_out.idx_to_dp := dict_in.idx_from_dp // use stored idx  
                        dict_out.symbol := data_in
    }   
        
/** Dict Control
  * responsible for interacting with the dictionary
  * Whereas LZWControl essentially implements the loop, this module implements the functionality of the LZW
  * dictionary entries are 12 bit index + 8 bit symbol
  */
class DictControl(
    val debug : Boolean = false, 
    val fetch_count : Int = 1,  // power of 2
    val dict_entry_size : Int = 20
) extends RTDesign():
    import DictCtrlState.*
    
    val lzw_out = LZWControlDictIn <> OUT.REG
    val lzw_in = LZWControlDictOut <> IN
    
    // Dictionary IF
    val dict_in = Bits(fetch_count * dict_entry_size) <> IN // incoming data (dict entries)
    val dict_in_valid = Bit <> IN // in case memories are multi cycle
    val dict_out = Bits(dict_entry_size) <> OUT // to efficiently use memories this might need to be changed
    val addr = Bits(12) <> OUT
    val en = Bit <> OUT // just enable, memory if figure out which memory
    val rd = Bit <> OUT
    
    // Vars
    val state = DictCtrlState <> VAR.REG init Idle
    val matching = Bits(fetch_count) <> VAR
    val addr_r = Bits(12) <> VAR.REG init all(0)
    val idx_r = Bits(12) <> VAR.REG init all(0)
    val sym_r = Bits(8) <> VAR.REG init all(0)
    val entry_count = UInt(13) <> VAR.REG init 256
    val found = Bit <> VAR.REG init false
    val matching_r = Bits(fetch_count) <> VAR.REG
    val fc_int = UInt(12) <> VAR
    val idx_found = Bits(12) <> VAR
    fc_int := fetch_count
    val fc_bits = ceil(log(fetch_count.toDouble)/log(2)).toInt
    val addr_tmp = Bits(12) <> VAR
    
    matching := all(0)
    idx_found := all(0)
    en := 0
    rd := 0
    lzw_out.done.din := 0
    lzw_out.found.din := 0
    dict_out := all(0)
    addr_tmp := all(0)
    state match
        case Idle => 
            if (lzw_in.start)
                sym_r.din := lzw_in.symbol
                idx_r.din := lzw_in.idx_to_dp
                matching_r.din := all(0)
                
                // first symbol, simply return
                if (lzw_in.idx_to_dp == h"FFF")
                    lzw_out.done.din := 1
                    lzw_out.idx_from_dp.din := (h"1", lzw_in.symbol)
                    lzw_out.found.din := 1
                else
                    // first byte found before
                    if (lzw_in.idx_to_dp.uint < 256)
                        addr := h"100"
                        addr_r.din := h"100" + fc_int
                    else
                        addr := lzw_in.idx_to_dp
                        addr_tmp := lzw_in.idx_to_dp >> fc_bits
                        addr_r.din := (addr_tmp << fc_bits) + fc_int
                    state.din := Search
                    rd := 1
                    
        case Search => 
            // check if symbol and index match and if address is lower than number of entries
            
            for (i <- 0 until fetch_count) 
                if (dict_in(20 * (i+1) - 1, 20 * i) == (idx_r, sym_r) && addr_r.resize(13) + i - fetch_count < entry_count) // check address vs entry_count here)
                    matching(i) := true
                    idx_found := addr_r + i - fetch_count // can repurpose the idx register when a match is found, store the addr/index where match ocurred
        
            if (dict_in_valid)
                addr := addr_r
                addr_r.din := addr_r.uint + fc_int
                rd := 1
                
                if (matching != b"0".resize(fetch_count))    
                    state.din := Idle
                    lzw_out.done.din := 1
                    lzw_out.found.din := true
                    lzw_out.idx_from_dp.din := idx_found
                    
                else if ((matching == b"0".resize(fetch_count)) && 
                    ((addr_r.resize(13) + fc_int > entry_count) || (addr_r.resize(13) + fc_int < 256)))
                    if (entry_count < 4096)
                        state.din := AddEntry
                    else
                        state.din := Idle
                        lzw_out.done.din := 1
                        lzw_out.found.din := 0
                        lzw_out.idx_from_dp.din := all(0)
        
        case AddEntry =>
            entry_count.din := entry_count + 1
            addr := (entry_count).bits(11, 0) // addr is simply the next
            en := 1
            dict_out := (idx_r, sym_r)
            
            if (dict_in_valid) // continue to response once write is confirmed
                state.din := Idle
                lzw_out.done.din := 1
                lzw_out.found.din := 0
                lzw_out.idx_from_dp.din := all(0)
                
    
    
// Control and Status Register
class LZWCSR() extends RTDesign:
    val data_in = Bits(32) <> IN // data written to the CSR
    val wr = Bit <> IN // write enable
    val rd = Bit <> IN // rd enable
    val addr = Bits(1) <> IN 
    val data_out = Bits(32) <> OUT.REG
    
    val status = Bits(32) <> IN
    val control = Bits(32) <> OUT // Control info for the LZW

    val csr_r = Bits(32) <> VAR.REG

    control := csr_r
    if (status(0))
        csr_r.din := (csr_r(31, 1), 0)
    
    if (wr)
        if (addr.uint == 0)
            csr_r.din := data_in
    else if (rd)
        if (addr.uint == 0)
            data_out.din := csr_r
        if (addr.uint == 1)
            data_out.din := status
        
class LZWnDict(val fetch_count : Int = 1) extends RTDesign():
    val valid_in = Bit <> IN
    val data_in = Bits(8) <> IN
    val lzw_ready = Bit <> OUT
    
    val valid_out = Bit <> OUT
    val data_out = Bits(12) <> OUT
    val out_ready = Bit <> IN
    
    val dict_new_entry = Bits(20) <> OUT
    val dict_data_valid = Bit <> IN
    val dict_entries = Bits(20 * fetch_count) <> IN
    val dict_wr_en = Bit <> OUT
    val dict_ctrl_rd = Bit <> OUT
    val addr_to_dict = Bits(12) <> OUT
    
    val csr_clear_finish = Bit <> OUT
    val csr_set_finish = Bit <> IN
    val control = Bits(32) <> IN
    val status = Bits(32) <> OUT
    
    val LZWctrl = new LZWControl
    val DICTctrl = new DictControl(fetch_count = fetch_count)
    
    val reg_to_get_clk_and_rst = Bit <> VAR.REG
    
    // LZW to Dict
    LZWctrl.dict_in <> DICTctrl.lzw_out
    LZWctrl.dict_out <> DICTctrl.lzw_in
    
    // Dict to parent
    dict_entries <> DICTctrl.dict_in
    DICTctrl.dict_in_valid <> dict_data_valid
    DICTctrl.dict_out <> dict_new_entry
    DICTctrl.addr <> addr_to_dict
    DICTctrl.en <> dict_wr_en
    
    // LZW to top
    LZWctrl.clear_finish <> csr_clear_finish
    LZWctrl.data_in <> data_in
    LZWctrl.data_out <> data_out
    LZWctrl.valid_in <> valid_in
    LZWctrl.valid_out <> valid_out
    LZWctrl.ready_in <> lzw_ready
    LZWctrl.ready_out <> out_ready
    LZWctrl.finish <> csr_set_finish
    LZWctrl.control <> control
    LZWctrl.status <> status
    DICTctrl.rd <> dict_ctrl_rd
    

class LZWtle() extends RTDesign(RTcfg):
    val valid_in = Bit <> IN
    val data_in = Bits(8) <> IN
    val ready_in = Bit <> OUT
    
    val valid_out = Bit <> OUT
    val data_out = Bits(12) <> OUT
    val ready_out = Bit <> IN
    
    val CSR_din = Bits(32) <> IN
    val CSR_dout = Bits(32) <> OUT
    val CSR_read = Bit <> IN
    val CSR_write = Bit <> IN
    val CSR_address = Bits(ceil(log(1+1)/log(2)).toInt) <> IN
    
    val LZW = new LZWnDict(fetch_count = 1) 
    val CSR = new LZWCSR()
    val MemIF = new LZWSimpleMemIF()
    val Mem = new SimpleMem(20, 4096)
    
    // to parent
    LZW.valid_in <> valid_in
    LZW.valid_out <> valid_out
    LZW.lzw_ready <> ready_in
    LZW.out_ready <> ready_out
    LZW.data_in <> data_in
    LZW.data_out <> data_out
    LZW.addr_to_dict <> MemIF.addr
    CSR_din <> CSR.data_in
    CSR_dout <> CSR.data_out
    CSR_read <> CSR.rd
    CSR_write <> CSR.wr
    CSR_address <> CSR.addr
    LZW.csr_set_finish <> CSR.control(0)
    
    // Mem to Interface
    Mem.addr <> MemIF.addr_to_mem
    Mem.en <> MemIF.en_to_mem
    Mem.data_in <> MemIF.new_entry_to_mem
    MemIF.entries_from_mem <> Mem.data_out
    
    // IF to LZW
    MemIF.en <> LZW.dict_wr_en
    MemIF.rd <> LZW.dict_ctrl_rd
    MemIF.new_entry <> LZW.dict_new_entry
    MemIF.entries <> LZW.dict_entries
    LZW.dict_data_valid <> MemIF.entries_valid 
    
    // CSR to LZW
    CSR.control <> LZW.control
    CSR.status <> (LZW.status(31, 1), LZW.csr_clear_finish)
