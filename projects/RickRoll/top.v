`timescale 1ns/1ps

module top;
   reg clock;
   reg clock90;
   reg reset;

   wire dac_sck;
   wire dac_cs;
   wire dac_clr;
   wire dac_mosi;

   wire dis_spi_ss_b;
   wire dis_amp_cs;
   wire dis_conv;
   wire dis_sf_ce0;
   wire dis_fpga_init_b;
   
   initial begin
      $dumpfile("./projects/RickRoll/RickRoll.vcd");
      $dumpvars;
            
      #400000 $finish;
   end

   initial begin
      #0 clock = 0;
      
      forever #2 clock = ~clock;
   end

   initial begin
      #1 clock90 = 0;

      forever #2 clock90 = ~clock90;
   end

   initial begin
      #0 reset = 0;
      #2 reset = 1;
      #6 reset = 0;
   end

   RR_Module rr(clock,clock90,reset,dac_sck,dac_cs,dac_clr,dac_mosi,dis_spi_ss_b,dis_amp_cs,dis_conv,dis_sf_ce0,dis_fpga_init_b);
endmodule // top
