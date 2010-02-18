`timescale 1ns/1ps

module top;
   reg clock;
   reg reset;

   initial begin
      clock = 0;
      
      forever #2 clock = ~clock;
   end

   initial begin
      #0 reset = 0;
      #2 reset = 1;
      #4 reset = 0;
   end
   
   RR_Module rr(clock,reset);
endmodule // top
