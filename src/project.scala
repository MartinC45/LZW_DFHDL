// NOTE using local DFHDL version corresponding to newest Github version

//> using scala 3.4.2
//> using dep io.github.dfianthdl::dfhdl::0.4.3+87-f49f2407-SNAPSHOT
//> using plugin io.github.dfianthdl:::dfhdl-plugin:0.4.3+87-f49f2407-SNAPSHOT
//> using option -deprecation -language:implicitConversions

import dfhdl.*

given options.CompilerOptions.Backend = backends.vhdl.v2008
given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Sync, RstCfg.Active.Low)
given options.CompilerOptions.PrintDesignCodeAfter = false

@main def main = 
   LZWtle().compile
