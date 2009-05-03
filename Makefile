%.com : %.scm
	echo '(cf' "\"$^\"" ')' | mit-scheme

hamil.com : hamil.scm

.PHONY : run clean
run : hamil.com
	mit-scheme --load hamil --eval "(hamil us-graph 'me)"

clean : 
	$(RM) hamil.com hamil.bci hamil.bin
