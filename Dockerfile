FROM clojure:lein

WORKDIR /code

COPY . /code

RUN lein install

EXPOSE 8081

CMD ["lein", "with-profile", "+docker", "run"]
