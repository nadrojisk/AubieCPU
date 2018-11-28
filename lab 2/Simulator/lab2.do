add wave -position insertpoint  \
sim:/alu/operand1 \
sim:/alu/operand2 \
sim:/alu/operation \
sim:/alu/result \
sim:/alu/error


force -freeze sim:/alu/operand1 32'h00BBBBBB 0
force -freeze sim:/alu/operand2 32'h00222222 0
force -freeze sim:/alu/operation 4'h0 0
run 50 ns

force -freeze sim:/alu/operand1 32'hFFFFFFFF 0
force -freeze sim:/alu/operand2 32'h00000001 0
force -freeze sim:/alu/operation 4'h0 0
run 50 ns

force -freeze sim:/alu/operand1 32'hAAAAAAAA 0
force -freeze sim:/alu/operand2 32'h11111111 0
force -freeze sim:/alu/operation 4'h1 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000001 0
force -freeze sim:/alu/operand2 32'hFFFFFFFF 0
force -freeze sim:/alu/operation 4'h1 0
run 50 ns

force -freeze sim:/alu/operand1 32'h55555555 0
force -freeze sim:/alu/operand2 32'h22222222 0
force -freeze sim:/alu/operation 4'h2 0
run 50 ns

force -freeze sim:/alu/operand1 32'h0FFFFFFF 0
force -freeze sim:/alu/operand2 32'h0FFFFFFF 0
force -freeze sim:/alu/operation 4'h2 0
run 50 ns

force -freeze sim:/alu/operand1 32'h80000000 0
force -freeze sim:/alu/operand2 32'h80000000 0
force -freeze sim:/alu/operation 4'h2 0
run 50 ns

force -freeze sim:/alu/operand1 32'hAAAAAAAA 0
force -freeze sim:/alu/operand2 32'hEEEEEEEE 0
force -freeze sim:/alu/operation 4'h3 0
run 50 ns

force -freeze sim:/alu/operand1 32'h7FFFFFFF 0
force -freeze sim:/alu/operand2 32'h80000000 0
force -freeze sim:/alu/operation 4'h3 0
run 50 ns

force -freeze sim:/alu/operand1 32'h80000000 0
force -freeze sim:/alu/operand2 32'h7FFFFFFF 0
force -freeze sim:/alu/operation 4'h3 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000002 0
force -freeze sim:/alu/operand2 32'h00000008 0
force -freeze sim:/alu/operation 4'h4 0
run 50 ns

force -freeze sim:/alu/operand1 32'h80000000 0
force -freeze sim:/alu/operand2 32'h80000001 0
force -freeze sim:/alu/operation 4'h4 0
run 50 ns

force -freeze sim:/alu/operand1 32'h80000000 0
force -freeze sim:/alu/operand2 32'h7FFFFFFE 0
force -freeze sim:/alu/operation 4'h4 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000024 0
force -freeze sim:/alu/operand2 32'h00000002 0
force -freeze sim:/alu/operation 4'h5 0
run 50 ns

force -freeze sim:/alu/operand1 32'h80000000 0
force -freeze sim:/alu/operand2 32'hFFFFFFFF 0
force -freeze sim:/alu/operation 4'h5 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000024 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'h5 0
run 50 ns

force -freeze sim:/alu/operand1 32'hCCCC0000 0
force -freeze sim:/alu/operand2 32'h00006666 0
force -freeze sim:/alu/operation 4'h6 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000000 0
force -freeze sim:/alu/operand2 32'h00000006 0
force -freeze sim:/alu/operation 4'h6 0
run 50 ns

force -freeze sim:/alu/operand1 32'hCCCCCCCC 0
force -freeze sim:/alu/operand2 32'h66666666 0
force -freeze sim:/alu/operation 4'h7 0
run 50 ns

force -freeze sim:/alu/operand1 32'hFFFFFFFF 0
force -freeze sim:/alu/operand2 32'h66666666 0
force -freeze sim:/alu/operation 4'h7 0
run 50 ns

force -freeze sim:/alu/operand1 32'h0000CCCC 0
force -freeze sim:/alu/operand2 32'h66660000 0
force -freeze sim:/alu/operation 4'h8 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000000 0
force -freeze sim:/alu/operand2 32'h11111111 0
force -freeze sim:/alu/operation 4'h8 0
run 50 ns

force -freeze sim:/alu/operand1 32'h0000000C 0
force -freeze sim:/alu/operand2 32'h00000006 0
force -freeze sim:/alu/operation 4'h9 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000008 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'hA 0
run 50 ns

force -freeze sim:/alu/operand1 32'h00000000 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'hA 0
run 50 ns

force -freeze sim:/alu/operand1 32'h0000000C 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'hB 0
run 50 ns

force -freeze sim:/alu/operand1 32'hFFFFFFFF 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'hB 0
run 50 ns

force -freeze sim:/alu/operand1 32'h22222222 0
force -freeze sim:/alu/operand2 32'h00000000 0
force -freeze sim:/alu/operation 4'hC 0
run 50 ns
