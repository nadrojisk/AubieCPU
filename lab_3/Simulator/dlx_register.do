add wave -position insertpoint  \
sim:/dlx_register/in_val  \
sim:/dlx_register/clock  \
sim:/dlx_register/out_val  \

force -freeze sim:/dlx_register/in_val 32'hFFFFFFFF 0
force -freeze sim:/dlx_register/clock 1 {}
run

force -freeze sim:/dlx_register/in_val 32'h22222222 0
force -freeze sim:/dlx_register/clock 0 {}
run

force -freeze sim:/dlx_register/clock 1 {}
run