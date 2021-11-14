object Test {
  @main def main: Unit = {
    import cats.Monad
    import cats.effect.unsafe.implicits.global
    import cats.effect.{IO, Resource, Spawn}
    import cats.implicits.given
    import com.comcast.ip4s.SocketAddress
    import fs2.io.net.Network

    val portNumber = 31629
    val serverAddress = SocketAddress.fromString(s"localhost:$portNumber").get
    val clientResource: Resource[IO, fs2.io.net.Socket[IO]] =
      for {
        // we prepare a tcp server which returns 0 for every read from a client
        _ <- Spawn[IO].background {
          Network[IO]
            .server(address = Some(serverAddress.host), port = Some(serverAddress.port))
            .evalMap(socket =>
              Monad[IO].whileM_(socket.isOpen)(socket.write(fs2.Chunk[Byte](0)))
            )
            .compile
            .drain
        }
        socket <- Network[IO].client(serverAddress)
      } yield socket

    val program: IO[Unit] =
      clientResource.use { client =>
        Monad[IO].foreverM {
          client.read(1024).start.flatMap { fiber =>
            fiber.cancel >> client.read(1024) >> fiber.join
          }
        }
      }.void

    program.unsafeRunSync()
  }
}
