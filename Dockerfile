FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

RUN apk add yarn

WORKDIR /app
COPY sync-server.sln .
COPY nuget.config .
COPY core/*.fs core/
COPY core/*.fsproj core/
COPY app/*.fs app/
COPY app/*.fsproj app/
COPY test/*.fs test/
COPY test/*.fsproj test/

COPY ["web/webpack.config.js", "web/yarn.lock", "web/package.json", "web/"]
COPY web/public web/public
COPY web/src/*.fs web/src/
COPY web/src/*.fsproj web/src/
COPY web/.config web/.config

RUN dotnet test
RUN dotnet publish -c Release -r linux-musl-x64 --self-contained false
RUN cd web && yarn && yarn build --mode production

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

EXPOSE 8080

WORKDIR /app
COPY --from=0 /app/app/bin/Release/net6.0/linux-musl-x64/publish app
COPY --from=0 /app/web/public web/public

WORKDIR /app/app

ENTRYPOINT ["dotnet", "app.dll", "s"]
