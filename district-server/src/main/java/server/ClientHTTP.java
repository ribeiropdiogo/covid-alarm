package server;

import common.ResourceInterface;
import netscape.javascript.JSObject;
import org.glassfish.jersey.client.ClientConfig;

import common.District;
import org.jboss.resteasy.client.jaxrs.ResteasyClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClientBuilder;
import org.jboss.resteasy.client.jaxrs.ResteasyWebTarget;
import zmq.io.net.Address;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

public class ClientHTTP {
    final ResteasyClient client;
    final ResteasyWebTarget target;

    ClientHTTP(String address) {
        //System.out.println("Endereco:" + address);
        String uri = "http://" + address + "/";
        client = new ResteasyClientBuilder().build();
        target = client.target(uri);
    }

    public void post(District district) {
        ResourceInterface client = target.proxy(ResourceInterface.class);
        Response response = client.post(district.getName(), district);
        System.out.println("Response: "  + response.getStatusInfo());
        System.out.println("Response post: " + response.readEntity(District.class).toString());
        response.close();
    }

    public void put(District district) {
        ResourceInterface client = target.proxy(ResourceInterface.class);
        Response response = client.put(district.getName(), district);
        System.out.println("Response Status: "  + response.getStatusInfo());
        System.out.println("Response put: " + response.readEntity(District.class).toString());
        response.close();
    }
}