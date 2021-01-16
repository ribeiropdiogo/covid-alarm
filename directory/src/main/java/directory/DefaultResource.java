package directory;

import common.District;
import common.ResourceInterface;

import com.codahale.metrics.annotation.Timed;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.*;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DefaultResource implements ResourceInterface {

    /*
      Retrieves all districts
   */
    @GET
    @Timed
    @Path("/")
    @Override
    public Response getDistricts() {
        List<District> result = new ArrayList<>();
        try {
            result = Data.allDistricts();
            if (!result.isEmpty()) {
                return Response.ok(result).build();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }

    /*
       Retrieves the number of users given a certain district's name
    */
    @GET
    @Timed
    @Path("district/getTotalUsers")
    @Override
    public Response getUsersPerDistrict(@QueryParam("district") String districtName){
        Integer result = 0;
        try {
            result = Data.getUsersPerDistrict(districtName);
            if (result != 0 ){
                return Response.ok(result).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }

    /*
       Retrieves the number of users infected in a district
    */
    @GET
    @Timed
    @Path("district/getTotalInfected")
    @Override
    public Response getTotalInfectedPerDistrict(@QueryParam("district") String districtName){
        Integer result = 0;
        try {
            result = Data.getInfectedPerDistrict(districtName);
            if (result != 0 ){
                return Response.ok(result).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }

    /*
   Retrieves the list of top 5 districts with more users infected per total users
    */
    @GET
    @Timed
    @Path("/getRacioMostInfected")
    @Override
    public Response getMostInfected(){
        Map<String, Float> result = new HashMap<>();
        try {
            result = Data.getRacioMostInfected();
            if (!result.isEmpty()){
                return Response.ok(result).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }

    /*
   Retrieves the list of top 5 crowded locations
    */
    @GET
    @Timed
    @Path("/getMostCrowded")
    @Override
    public Response getMostCrowded() {
        List<String> result = new ArrayList<>();
        try {
            result = Data.top5CrowdedLocation();
            if (!result.isEmpty() ){
                return Response.ok(result).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }

    /*
    Retrieves the mean of users that have been together with an infected user
     */
    @GET
    @Timed
    @Path("/getUsersMeetInfected")
    @Override
    public Response getUsersMeetInfected() {
        float result = 0;
        try {
            result = Data.meanMeetInfected();
            if (result != 0 ){
                return Response.ok(result).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_FOUND).build();
    }


    /*
    To update an existing district, we’re going to use the HTTP PUT method.
    */
    @PUT
    @Path("/district/{name}")
    @Override
    public Response put(@PathParam("name") String name, District district) {
        District d = new District();

        try {
            synchronized (this) {
               if(district == null)
                   return Response.status(400).entity("Please add District details !!").build();
               if(name == null)
                   return Response.status(400).entity("Please provide the District name !!").build();

               else  d = Data.updateDistrictExceptName(name, district);

               if(d != null) {
                    return Response.ok(d).build();
               }
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
        return Response.ok(Response.Status.NOT_FOUND).build();
    }

    /*
    Removes a district
     */
    @DELETE
    @Path("/delete/{name}")
    @Override
    public Response delete(@PathParam("name") String name) {
        try {
            synchronized (this) {
                Data.removeDistrict(name);
                return Response.ok().build();
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
        return Response.ok(Response.Status.NOT_FOUND).build();
    }

    /*
    Creates a district
     */
    @POST
    @Path("/district/{name}")
    @Override
    public Response post(@PathParam("name") String name, District district) {
        try {
            synchronized (this) {
                if(district == null)
                    return Response.status(400).entity("Please add District details !!").build();
                if(name == null)
                    return Response.status(400).entity("Please provide the District name !!").build();

                System.out.println("Olá fiz um post de: " + district);

                District d = Data.addDistrict(district);

                if(!d.equals(null)) {
                    return Response.ok(d).build();
                }
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
        return Response.status(Response.Status.NOT_ACCEPTABLE).build();
    }
}