add wave -position end  sim:/reg_file/data_in
add wave -position end  sim:/reg_file/readnotwrite
add wave -position end  sim:/reg_file/clock
add wave -position end  sim:/reg_file/data_out
add wave -position end  sim:/reg_file/reg_number

force -freeze sim:/reg_file/data_in 32'h00000099 0
force -freeze sim:/reg_file/clock 1 0

run

force -freeze sim:/reg_file/data_in 32'h00000000 0
force -freeze sim:/reg_file/readnotwrite 1 0

run

force -freeze sim:/reg_file/data_in 32'hFFFFFFFF 0
force -freeze sim:/reg_file/readnotwrite 0 0
force -freeze sim:/reg_file/reg_number 5'h02 0

run

force -freeze sim:/reg_file/data_in 32'h00000000 0
force -freeze sim:/reg_file/readnotwrite 1 0

run

force -freeze sim:/reg_file/reg_number 5'h00 0
run

force -freeze sim:/reg_file/data_in 32'h00000022 0
force -freeze sim:/reg_file/clock 0 0
force -freeze sim:/reg_file/readnotwrite 0 0

run

force -freeze sim:/reg_file/readnotwrite 1 0

run