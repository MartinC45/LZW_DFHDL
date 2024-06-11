//> using scala 3.4.2
//> using dep io.github.dfianthdl::dfhdl::0.4.3+87-f49f2407-SNAPSHOT
//> using plugin io.github.dfianthdl:::dfhdl-plugin:0.4.3+87-f49f2407-SNAPSHOT
//> using option -deprecation -language:implicitConversions


import dfhdl.*

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Enables printing the generated chosen backend code:
// given options.CompilerOptions.PrintGenFiles = true
// Uncomment to select vhdl compilation (default is verilog):
given options.CompilerOptions.Backend = backends.vhdl.v2008
// Uncomment to enable printing design code before compilation (after elaboration):
// given options.CompilerOptions.PrintDesignCodeBefore = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
// Uncomment to set different clock and reset configurations:
given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.Low)
given options.CompilerOptions.PrintDesignCodeAfter = false
////////////////////////////////////////////////////////////////////////////////////////////////
//The entry point to your compilation program starts here



@main def main = 

//    ShiftRegister(10, UInt(10), b"0000000000".uint).compile
//    ShiftRegister(10, Bit, false.bit).compile
//    FirFilter(8, Vector(1, 2, 3)).compile
//    LZWControl().compile
//    DictControl(fetch_count = 1).compile
//    LZWnDict().compile
   LZWtle().compile
//    SimpleMem(20, 4096).compile
//    DF_FSM().compile
//   DFfirfilter(10, Vector(1, 0, 1, 0, 1)).compile
//    GenFifo(16, Bits[16], h"0000").compile
//    DF_FSM(true).compile
//   FirFilterTest().compile

//    ClockRstConnection().compile
//    NoCfgClockRst().compile
//    DoubleStructDecl().compile
//    
//    
//    VhdlArray(10).compile
//    GenFifo(8, RGB).compile
//    IntegerIndexingIssue().compile
//    Width0Issue(1).compile
//    VerilogSRA().compile
//    FirstDF().compile
//    TestDF().compile
//    GenericCounterWithEn(64).compile.commit
//    GlobCounter(32).compile.commit
//    BoothMultiplier(4, 5).compile.commit
//    ShiftIssue().compile
//    TypeConvertIssue().compile
//    StdLogicConvIssue().compile
//    ArrayIssue().compile
//    StructConvIssue().compile
