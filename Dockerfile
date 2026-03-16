# Build stage
FROM eclipse-temurin:21-jdk AS build
WORKDIR /app
COPY . .
RUN sbt assembly

# Runtime stage
FROM eclipse-temurin:21-jre-alpine
WORKDIR /app
COPY --from=build /app/target/scala-3.3.7/binocular-assembly-*.jar binocular.jar
ENTRYPOINT ["java", "-jar", "binocular.jar"]
