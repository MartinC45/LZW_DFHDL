import dfhdl.*

enum LZWCtrlState extends Encode:
    case Idle, Receive

enum DictCtrlState extends Encode:
    case Idle, Search, AddEntry
