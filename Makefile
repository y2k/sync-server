start: start_web start_back

start_web:
	cd web && yarn start

start_back:
	cd app && dotnet watch run s

build:
	dotnet build

.PHONY: start start_web start_back build
