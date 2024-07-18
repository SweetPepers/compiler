# C编译器参数：使用C11标准，生成debug信息，禁止将未初始化的全局变量放入到common段
CFLAGS=-std=c11 -g -fno-common
# 指定C编译器，来构建项目
CC=gcc
# C源代码文件，表示所有的.c结尾的文件
SRCS=$(wildcard *.c)
# C文件编译生成的未链接的可重定位文件，将所有.c文件替换为同名的.o结尾的文件名
OBJS=$(SRCS:.c=.o)
# 使用math库
LDFLAGS=-lm

# rvcc标签，表示如何构建最终的二进制文件，依赖于所有的.o文件
# $@表示目标文件，此处为rvcc，$^表示依赖文件，此处为$(OBJS)
rvcc: $(OBJS)
# 将多个*.o文件编译为rvcc
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# 所有的可重定位文件依赖于rvcc.h的头文件
$(OBJS): rvcc.h

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# Stage 1

# 只使用rvcc进行宏的测试
# test/macro.exe: rvcc test/macro.c
# 	./rvcc -c -o test/macro.o test/macro.c
# 	riscv64-linux-gnu-gcc -static -o $@ test/macro.o -xc test/common
#	$(RISCV)/bin/riscv64-unknown-linux-gnu-gcc -o $@ test/macro.o -xc test/common

# 测试标签，运行测试
test/%.exe: rvcc test/%.c
#	$(CC) -o- -E -P -C test/$*.c | ./rvcc -c -o test/$*.o -
	./rvcc -Itest -c -o test/$*.o test/$*.c
# riscv64-linux-gnu-gcc -o- -E -P -C test/$*.c | ./rvcc -o test/$*.s -
# $(CC) -static -o $@ test/$*.s -xc test/common
	riscv64-linux-gnu-gcc -static -o $@ test/$*.o -xc test/common

run/%: test/%.exe
	qemu-riscv64 -L $(RISCV)/sysroot test/$*.exe || exit 1

test: $(TESTS)
# for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	for i in $^; do echo $$i; qemu-riscv64 -L $(RISCV)/sysroot ./$$i || exit 1; echo; done
#	for i in $^; do echo $$i; $(RISCV)/bin/spike --isa=rv64gc $(RISCV)/riscv64-unknown-linux-gnu/bin/pk ./$$i || exit 1; echo; done
	test/driver.sh ./rvcc



# Stage 2

# 此时构建的stage2/rvcc是RISC-V版本的，跟平台无关

# stage2的汇编编译为可重定位文件
# stage2/%.o: stage2/%.s
# 	riscv64-linux-gnu-gcc -c stage2/$*.s -o stage2/$*.o  
# # 垃圾编译器 链接都做不到

stage2/rvcc: $(OBJS:%=stage2/%)
	riscv64-linux-gnu-gcc -o $@ $^ $(LDFLAGS)

# 利用stage1的rvcc去将rvcc的源代码编译为stage2的汇编文件
stage2/%.o: rvcc self.py %.c
	mkdir -p stage2/test
	./self.py rvcc.h $*.c > stage2/$*.c
	./rvcc -Itest -c -o stage2/$*.o stage2/$*.c


# 利用stage2的rvcc去进行测试
stage2/test/%.exe: stage2/rvcc test/%.c
	mkdir -p stage2/test
	$(CC) -o- -E -P -C test/$*.c | ./stage2/rvcc -c -o stage2/test/$*.o -
	riscv64-linux-gnu-gcc -static -o $@ stage2/test/$*.o -xc test/common

# 只使用stage2的rvcc进行宏的测试
stage2/test/macro.exe: stage2/rvcc test/macro.c
	mkdir -p stage2/test
	./stage2/rvcc -c -o stage2/test/macro.o test/macro.c
	riscv64-linux-gnu-gcc -o $@ stage2/test/macro.o -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; qemu-riscv64 -L $(RISCV)/sysroot ./$$i || exit 1; echo; done
	test/driver.sh ./stage2/rvcc


# 进行全部的测试
test-all: test test-stage2

# 清理标签，清理所有非源代码文件
clean:
	rm -rf rvcc tmp* $(TESTS) test/*.s test/*.exe stage2/
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
# 伪目标，没有实际的依赖文件
.PHONY: test clean test-stage2