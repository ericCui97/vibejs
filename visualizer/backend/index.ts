import { join } from "path";
import { write } from "bun";

const COMPILER_BIN = join(import.meta.dir, "../../js-compiler/target/debug/js-compiler");

const server = Bun.serve({
  port: 3000,
  async fetch(req) {
    const url = new URL(req.url);
    
    // CORS headers
    const headers = {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "POST, GET, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
    };

    if (req.method === "OPTIONS") {
      return new Response(null, { headers });
    }

    if (url.pathname === "/compile" && req.method === "POST") {
        try {
            const body = await req.json() as any;
            const sourceCode = body.code || "";
            const mode = body.mode || "token"; // "token", "ast", or "eval"
            
            // Write temp file
            const tempFile = join(import.meta.dir, "temp.js");
            await write(tempFile, sourceCode);

            // Run compiler
            const args = [COMPILER_BIN, tempFile];
            if (mode === "ast") {
                args.push("--ast");
            } else if (mode === "eval") {
                args.push("--eval");
            } else {
                args.push("--json");
            }

            const proc = Bun.spawn(args, {
                stdout: "pipe",
                stderr: "pipe",
            });

            const output = await new Response(proc.stdout).text();
            const error = await new Response(proc.stderr).text();
            const exitCode = await proc.exited;

            if (exitCode !== 0) {
                 return new Response(JSON.stringify({ error: error || "Compilation failed" }), { 
                    headers, 
                    status: 400 
                });
            }

            // For eval mode, we return a JSON object with logs
            if (mode === "eval") {
                const combinedHeaders = { ...headers, "Content-Type": "application/json" };
                return new Response(JSON.stringify({ logs: output }), { headers: combinedHeaders });
            }

            const combinedHeaders = { ...headers, "Content-Type": "application/json" };
            return new Response(output, { headers: combinedHeaders });

        } catch (e) {
             return new Response(JSON.stringify({ error: String(e) }), { headers, status: 500 });
        }
    }

    return new Response("Bun Visualizer Backend Running", { headers });
  },
});

console.log(`Listening on http://localhost:${server.port} ...`);
