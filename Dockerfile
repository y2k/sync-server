FROM mcr.microsoft.com/dotnet/sdk:6.0.201-alpine3.15-amd64

RUN apk add yarn

WORKDIR /app
COPY sync-server.sln .
COPY nuget.config .
COPY src/*.fs src/
COPY src/*.fsproj src/
COPY test/*.fs test/
COPY test/*.fsproj test/

COPY ["client/webpack.config.js", "client/yarn.lock", "client/package.json", "client/"]
COPY client/public client/public
COPY client/src/*.fs client/src/
COPY client/src/*.fsproj client/src/
COPY client/.config client/.config

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false
RUN cd client && yarn && yarn build --mode production

FROM mcr.microsoft.com/dotnet/runtime:6.0.3-alpine3.15-amd64

EXPOSE 8080

WORKDIR /app
COPY --from=0 /app/src/bin/Release/net6.0/linux-x64/publish app
COPY --from=0 /app/client/public client/public

WORKDIR /app/app

ENTRYPOINT ["dotnet", "app.dll", "s"]
