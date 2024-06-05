COMPILER = dune
COMPILE = $(COMPILER) build

EXEC = latsi

all : $(EXEC)
	@echo "\tPour exécuter $(EXEC), exécuter \001\033[1;35m\002./$(EXEC) <fichier_source>\001\033[0m\002."

clean :
	@$(COMPILER) clean
	@rm -f $(EXEC)
	
	@echo "\t\001\033[34m\002projet nettoyé\001\033[0m\002"
	@echo "\tpour compiler le projet : \001\033[1;35m\002make\001\033[0m\002"

$(EXEC):
	@$(COMPILE)
	@cp _build/default/main.exe ./latsi
