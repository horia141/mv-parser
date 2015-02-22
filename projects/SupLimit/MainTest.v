`timescale 1ns/100ps

module MainTest;
   reg clock;
   reg reset;
   wire [7:0] leds;

   initial begin
      $dumpfile("./projects/SupLimit/SupLimit.vcd");
      $dumpvars;

      #1000000 $finish;
   end

   initial begin
      #0 clock = 0;
      forever #1 clock = ~clock;
   end

   initial begin
      #0 reset = 0;
      #2 reset = 1;
      #4 reset = 0;
   end

   CountSupLimit
   countSupLimit(.clock(clock),
		 .reset(reset),
		 .leds(leds));
endmodule // MainTest
