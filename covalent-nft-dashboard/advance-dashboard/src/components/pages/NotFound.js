import { Link } from "react-router-dom";

const NotFound = () => {
  return (
    <div className="text-center text-6xl pt-10" style={{ minHeight: "100vh" }}>
      <p className="text-white">Coming Soon!🙂</p>
      <p className="text-sm bg-white rounded w-2/12 mx-auto mt-5 py-4">
        {" "}
        <Link to="/"> Back to Home</Link>
      </p>
    </div>
  );
};

export default NotFound;
