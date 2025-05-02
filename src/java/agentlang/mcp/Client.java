package agentlang.mcp;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.time.Duration;
import com.fasterxml.jackson.databind.ObjectMapper; 
import com.fasterxml.jackson.databind.ObjectWriter;
import io.modelcontextprotocol.spec.McpSchema.*;
import io.modelcontextprotocol.spec.McpClientTransport;
import io.modelcontextprotocol.client.transport.ServerParameters;
import io.modelcontextprotocol.client.McpClient;
import io.modelcontextprotocol.client.McpSyncClient;
import io.modelcontextprotocol.client.transport.StdioClientTransport;

public class Client {
    public static Map<String, Object> makeSyncClient(McpClientTransport transport) {
	McpSyncClient client = McpClient.sync(transport)
	    .requestTimeout(Duration.ofSeconds(10))
	    .capabilities(ClientCapabilities.builder()
			  .roots(true)      // Enable roots capability
			  .build())
	    .build();

	client.initialize();

	ListToolsResult tools = client.listTools();
	Map<String, Object> toolsSpec = new HashMap<>();
	for (Tool t : tools.tools()) {
	    String schemaJson;
	    try {
		ObjectWriter ow = new ObjectMapper().writer().withDefaultPrettyPrinter();
		schemaJson = ow.writeValueAsString(t.inputSchema());
	    } catch (Exception ex) {
		schemaJson = "{}";
	    }
	    toolsSpec.put(t.name(), Map.of("description", t.description(),
					   "schema", schemaJson));
	}
	return Map.of("handle", client, "tools", toolsSpec);
    }

    public static String callTool(McpSyncClient client, String toolName, Map<String, Object> toolParams) {
	CallToolResult result = client.callTool(new CallToolRequest(toolName, toolParams));
	if (result.isError() == Boolean.TRUE) {
	    return null;
	} else {
	    StringBuilder sb = new StringBuilder("");
	    for (Content c : result.content()) {
		if (c.type() == "text") {
		    TextContent tc = (TextContent) c;
		    sb.append(tc.text());
		}
	    }
	    return sb.toString();
	}
    }

    public static boolean close(McpSyncClient client) {
	if (client != null) {
	    client.closeGracefully();
	    return true;
	}
	return false;
    }
		
    public static McpClientTransport makeTransport(String cmd, List<String> cmdArgs) {
	String[] cmd_args = cmdArgs.toArray(new String[0]);
	ServerParameters params = ServerParameters.builder(cmd)
	    .args(cmd_args)
	    .build();
	return new StdioClientTransport(params);
    }
}
