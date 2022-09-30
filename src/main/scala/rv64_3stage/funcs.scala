package sodor.rv64

import chisel3._
import chisel3.util._

object funcs {
  /**
    * Sign-extends data
    * @param data input data
    * @param out_width output width
    * @return sign-extended data
    */
  def sign_ext(data: UInt, out_width: Int): UInt = {
    val in_width = data.getWidth
    require(in_width < out_width, s"${out_width} must be wider than ${in_width}")
    val sign: Bool = data(in_width-1)
    Cat(Fill(out_width-in_width, sign), data)
  }
}
