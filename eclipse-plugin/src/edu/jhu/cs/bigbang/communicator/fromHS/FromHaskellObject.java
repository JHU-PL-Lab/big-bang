package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.communicator.util.CommunicatorSerializable;

//TODO make it into an interface
// two implementations : 1. Result(abstract class) which should have the cmdId
//                       2. FHError(abstract class) which should also have the cmdId and extends exception   
public interface FromHaskellObject extends CommunicatorSerializable{
}
