FROM mcr.microsoft.com/dotnet/sdk:6.0

WORKDIR /app

COPY sync-server.sln .
COPY nuget.config .
COPY src/*.fs src/
COPY src/*.fsproj src/
COPY test/*.fs test/
COPY test/*.fsproj test/
COPY client/src/*.fs client/src/
COPY client/src/*.fsproj client/src/

RUN ls -l

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:6.0

EXPOSE 8080

WORKDIR /app
COPY --from=0 /app/src/bin/Release/net6.0/linux-x64/publish .

ENTRYPOINT ["dotnet", "app.dll", "s"]
