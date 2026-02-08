// Fiber benchmark server - Go comparison for performance testing
//
// Implements the same endpoints as Baseline's bench_server.bl

package main

import (
	"fmt"
	"log"

	"github.com/gofiber/fiber/v2"
)

func main() {
	app := fiber.New(fiber.Config{
		// Optimize for performance
		DisableStartupMessage: false,
		Prefork:               false, // Set to true for multi-process mode
	})

	// GET /hello - plain text
	app.Get("/hello", func(c *fiber.Ctx) error {
		return c.SendString("Hello, World!")
	})

	// GET /json - JSON response
	app.Get("/json", func(c *fiber.Ctx) error {
		return c.JSON(fiber.Map{"message": "Hello, World!"})
	})

	// POST /echo - echo body
	app.Post("/echo", func(c *fiber.Ctx) error {
		return c.Send(c.Body())
	})

	// GET /health - health check
	app.Get("/health", func(c *fiber.Ctx) error {
		return c.JSON(fiber.Map{"status": "ok"})
	})

	// GET /users/:id - user lookup
	app.Get("/users/:id", func(c *fiber.Ctx) error {
		id := c.Params("id")
		return c.JSON(fiber.Map{
			"id":   id,
			"name": fmt.Sprintf("User %s", id),
		})
	})

	fmt.Println("Fiber server listening on http://0.0.0.0:8082")
	fmt.Println("Endpoints:")
	fmt.Println("  GET  /hello     - plain text hello world")
	fmt.Println("  GET  /json      - JSON hello world")
	fmt.Println("  POST /echo      - echo request body")
	fmt.Println("  GET  /health    - health check")
	fmt.Println("  GET  /users/:id - user lookup")

	log.Fatal(app.Listen(":8082"))
}
