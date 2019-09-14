all: cg1 cg2 cg3 cg4 opt1 p s

clean:
	rm -f cg1 cg2 cg3 cg4 opt1 p s a.pdf

# Test each program
test: cg1 cg2 cg3 cg4 opt1 p s
	@echo "Running ./p"
	@./p

	@echo; echo "Running ./s 10"
	@./s 10

	@echo; echo 'Running ./cg1 "2+A*b-C/d"'
	@./cg1 "2+A*b-C/d"

	@echo; echo 'Running ./cg2 "2+A*b-C/d"'
	@./cg2 "2+A*b-C/d"

	@echo; echo 'Running ./cg3 "2+A*b-C/d"'
	@./cg3 "2+A*b-C/d"

	@echo; echo 'Running ./cg4 "2+A*b-C/d"'
	@./cg4 "2+A*b-C/d"

	@echo; echo 'Running ./opt1 "5*(3+A)-2*(3+A)"'
	@./opt1 "5*(3+A)-2*(3+A)"

# If you have pic and groff installed, you can run
# opt1 to produce PDF output
a.pdf: opt1
	./opt1 -d "5*(3+A)-2*(3+A)" | pic | groff  -Tpdf > a.pdf
