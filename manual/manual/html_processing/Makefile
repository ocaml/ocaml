all:
	dune exec src/process_manual.exe quiet
	dune exec src/process_api.exe overwrite quiet
	dune exec src/process_api.exe compiler overwrite quiet

clean:
	rm -rf ../webman
