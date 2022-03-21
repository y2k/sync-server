start: start_web start_back
	# (cd client && yarn start) & (cd src && dotnet run s)

start_web:
	cd client && yarn start

start_back:
	cd src && dotnet watch run s

.PHONY: start
