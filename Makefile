start:
	(cd web && yarn start) & \
	(cd app && dotnet watch run s)

build:
	dotnet build

cbuild:
	dotnet clean
	dotnet build

.PHONY: start start_web start_back build
