build:
	LLVM_SYS_150_PREFIX=/home/bluecore/projects/llvm/llvm-project/build/ cargo b

run:
	LLVM_SYS_150_PREFIX=/home/bluecore/projects/llvm/llvm-project/build/ cargo run

check:
	LLVM_SYS_150_PREFIX=/home/bluecore/projects/llvm/llvm-project/build/ cargo check
