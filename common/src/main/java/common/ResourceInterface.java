package common;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public interface ResourceInterface {
        @GET
        @Path("/")
        Response getDistricts() ;

        @GET
        @Path("district/getTotalUsers")
        Response getUsersPerDistrict(@QueryParam("district") String districtName);

        @GET
        @Path("district/getTotalInfected")
        Response getTotalInfectedPerDistrict(@QueryParam("district") String districtName);

        @GET
        @Path("/getRacioMostInfected")
        Response getMostInfected();

        @GET
        @Path("/getMostCrowded")
        Response getMostCrowded();

        @GET
        @Path("/getUsersMeetInfected")
        Response getUsersMeetInfected();

        @POST
        @Path("/district/{name}")
        Response post(@PathParam("name") String name, District district);

        @DELETE
        @Path("/delete/{name}")
        Response delete(@PathParam("name") String name);

        @PUT
        @Path("/district/{name}")
        Response put(@PathParam("name") String name, District district);
}
