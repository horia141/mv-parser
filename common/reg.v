module Reg(clock,reset,data_i,writeEn,data_o);
   parameter Size = 8;

   input wire [('d1) - ('b1):0]  clock;
   input wire [('d1) - ('b1):0]  reset;
   input wire [(Size) - ('b1):0] data_i;
   input wire [('d1) - ('b1):0]  writeEn;

   output reg [(Size) - ('b1):0] data_o;

   always @ (posedge clock) begin
      if (reset) begin
	 data_o <= {Size{1'b0}};
      end
      else begin
	if (writeEn) begin
	   data_o <= data_i;
	end
      end
   end
endmodule // Reg
