package directory;

import common.District;
import common.ResourceInterface;

import com.codahale.metrics.annotation.Timed;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.*;
import java.util.stream.Collectors;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DefaultResource implements ResourceInterface {

    @GET
    @Timed
    @Path("/")
    @Override
    public Response getDistricts() {
        List<District> result = new ArrayList<>();
        try {
            result = Data.allDistricts();
            if (result.isEmpty()) {
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }

    @GET
    @Timed
    @Path("district/getTotalUsers")
    @Override
    public Response getUsersPerDistrict(@QueryParam("district") String districtName){
        Integer result = 0;
        try {
            result = Data.getUsersPerDistrict(districtName);
            if (result == 0 ){
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }


    @GET
    @Timed
    @Path("district/getTotalInfected")
    @Override
    public Response getTotalInfectedPerDistrict(@QueryParam("district") String districtName){
        Integer result = 0;
        try {
            result = Data.getInfectedPerDistrict(districtName);
            if (result == 0 ){
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }

    @GET
    @Timed
    @Path("/getRacioMostInfected")
    @Override
    public Response getMostInfected(){
        List<District> result = new ArrayList<>();
        Map<String, Float> racio = new HashMap<>();
        try {
            result = Data.getRacioMostInfected();
            if (result.isEmpty()){
                return Response.status(Response.Status.NOT_FOUND).build();
            }
            else {
                for (District d : result) {
                    float value = ((float)d.getTotalInfected() / (float)d.getTotalUsers());
                    racio.put(d.getName(), value);
                }
                racio = racio.entrySet().stream().limit(5).collect(Collectors.toMap(e -> e.getKey(),
                        e -> e.getValue()));
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }

    @GET
    @Timed
    @Path("/getMostCrowded")
    @Override
    public Response getMostCrowded() {
        List<String> result = new ArrayList<>();
        try {
            result = Data.top5CrowdedLocation();
            if (result.isEmpty() ){
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }

    @GET
    @Timed
    @Path("/getUsersMeetInfected")
    @Override
    public Response getUsersMeetInfected() {
        float result = 0;
        try {
            result = Data.meanMeetInfected();
            if (result == 0 ){
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return Response.ok(result).build();
    }


    /*
    Keep in mind that no validation is performed. If none of the fields in the request body match the
    fields in the District class, a new instance of District will still be created but all of its instance
    fields will be null.
     */
    @POST
    @Path("/district/{name}")
    @Override
    public Response post(@PathParam("name") String name, District district) {
        District d = new District();

        try {
            synchronized (this) {
               if(district == null)
                   return Response.status(400).entity("Please add District details !!").build();
               if(name == null)
                   return Response.status(400).entity("Please provide the District name !!").build();

               else  d = Data.updateDistrictExceptName(name, district);

               if(!d.equals(null)) {
                    return Response.ok(d).build();
               }
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
        return Response.ok(Response.Status.NOT_FOUND).build();
    }


    @DELETE
    @Path("/delete/{name}")
    @Override
    public Response delete(@PathParam("name") String name) {
        try {
            synchronized (this) {
                Data.removeDistrict(name);
            }
        }catch (Exception e) {
            e.printStackTrace();
        }
        return Response.ok().build();
    }


    /*
    To update an existing district, we’re going to use the HTTP PUT method. PUT is an idempotent method
    which means that executing the same request multiple times does not create additional state on
    the server. As with retrieving a single district, we’re going to need to read the district's name
    from the path using the @PathParam annotation.
     */
    @PUT
    @Path("/district/{name}")
    @Override
    public Response put(@PathParam("name") String name, District district) {

        try {
            synchronized (this) {
                if(district == null)
                    return Response.status(400).entity("Please add District details !!").build();
                if(name == null)
                    return Response.status(400).entity("Please provide the District name !!").build();

                System.out.println("Olá fiz um put de: " + district);

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