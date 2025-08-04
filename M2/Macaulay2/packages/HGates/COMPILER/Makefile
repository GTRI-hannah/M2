# Makefile for SLP Compiler
# run make all in terminal
# run ./slp_compiler to generate test cases, then run
# $ gcc -o test1 test1_output.c matrix.c
# $ ./test1
# input appropriate variables

CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -g 
TARGET = slp_compiler
SOURCES = slp_compiler.c matrix.c 
HEADERS = matrix.h
OBJECTS = $(SOURCES:.c=.o)

# Default target
all: $(TARGET)

# Build the compiler
$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

# Object file rules
%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Run tests
test: $(TARGET)
	./$(TARGET)

# Compile and run a specific SLP file
# Usage: make run SLP=example.slp OUTPUT=example.c
run: $(TARGET)
	@if [ -z "$(SLP)" ]; then \
		echo "Usage: make run SLP=input.slp [OUTPUT=output.c]"; \
		exit 1; \
	fi
	@if [ -n "$(OUTPUT)" ]; then \
		./$(TARGET) $(SLP) $(OUTPUT); \
	else \
		./$(TARGET) $(SLP); \
	fi

# Compile and execute generated C code
# Usage: make execute SLP=example.slp
execute: $(TARGET)
	@if [ -z "$(SLP)" ]; then \
		echo "Usage: make execute SLP=input.slp"; \
		exit 1; \
	fi
	@echo "Compiling SLP..."
	./$(TARGET) $(SLP) temp_output.c
	@echo "Compiling generated C code..."
	$(CC) $(CFLAGS) -o temp_program temp_output.c matrix.c
	@echo "Running program..."
	./temp_program
	@echo "Cleaning up..."
	rm -f temp_output.c temp_program

# Create test SLP files
create_tests:
	@echo "Creating test SLP files..."
	@echo "C0 = '3'" > test1.slp
	@echo "I0 = 'x'" >> test1.slp
	@echo "I1 = 'y'" >> test1.slp
	@echo "R0 = I0+I0" >> test1.slp
	@echo "R1 = R0+I1" >> test1.slp
	@echo "R2 = matrix{R1} (1, 1)" >> test1.slp
	@echo "R3 = R2+C0" >> test1.slp
	
	@echo "I0 = 'x'" > test2.slp
	@echo "I1 = 'y'" >> test2.slp
	@echo "R0 = I0+I0" >> test2.slp
	@echo "R1 = matrix{R0, I0, I1, I1} (2, 2)" >> test2.slp
	@echo "R2 = matrix{I0, I1} (2, 1)" >> test2.slp
	@echo "R3 = matrixProduct(R1, R2)" >> test2.slp
	
	@echo "Test files created: test1.slp, test2.slp"

# Validate generated C code by compiling it
validate: $(TARGET)
	@echo "Validating compiler output..."
	./$(TARGET) > temp_validation.c 2>/dev/null || echo "No default test"
	@if [ -f temp_validation.c ]; then \
		$(CC) $(CFLAGS) -c temp_validation.c -o temp_validation.o && \
		echo "Generated C code compiles successfully!" || \
		echo "Generated C code has compilation errors!"; \
		rm -f temp_validation.c temp_validation.o; \
	fi

# Clean build artifacts
clean:
	rm -f $(OBJECTS) $(TARGET) *.o
	rm -f test*_output.c test*.slp
	rm -f temp_output.c temp_program temp_validation.c temp_validation.o

# Install (copy to /usr/local/bin)
install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

# Uninstall
uninstall:
	rm -f /usr/local/bin/$(TARGET)

# Help
help:
	@echo "SLP Compiler Makefile"
	@echo "====================="
	@echo ""
	@echo "Targets:"
	@echo "  all           - Build the compiler (default)"
	@echo "  test          - Run built-in test cases"
	@echo "  run           - Compile specific SLP file"
	@echo "                  Usage: make run SLP=file.slp [OUTPUT=file.c]"
	@echo "  execute       - Compile SLP, build and run the result"
	@echo "                  Usage: make execute SLP=file.slp"
	@echo "  create_tests  - Create sample SLP test files"
	@echo "  validate      - Check if generated C code compiles"
	@echo "  clean         - Remove build artifacts"
	@echo "  install       - Install to /usr/local/bin"
	@echo "  uninstall     - Remove from /usr/local/bin"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make"
	@echo "  make test"
	@echo "  make create_tests"
	@echo "  make run SLP=test1.slp OUTPUT=compiled.c"
	@echo "  make execute SLP=test2.slp"

.PHONY: all test run execute create_tests validate clean install uninstall help