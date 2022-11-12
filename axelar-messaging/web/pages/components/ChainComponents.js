import { ChainList } from "../../utils/ChainList";

export const ChainOptions = (cb) => {
    return ChainList.map((option) => (
        <li key={`chain-option-${option.name}`} className="bg-black" onClick={() => cb(option)}>
            <a>{option.name}</a>
        </li>
    ));
};
export const ChainCard = ({ name, imgUrl }, cb) => {
    return (
        <div>
            <br />
            <br />
            <div className="flex justify-center avatar">
                <div className="w-24 rounded-full ring ring-primary ring-offset-base-100 ring-offset-2">
                    <img src={imgUrl} alt="Logo" />
                </div>
            </div>
            <div className="card-body">
                <div className="card-actions justify-center">
                    <div className="dropdown dropdown-bottom">
                        <label tabIndex="0" className="btn m-1">
                            {name}
                        </label>
                        <ul
                            tabIndex="0"
                            className="dropdown-content menu p-2 rounded-box w-52 h-44 overflow-scroll"
                        >
                            {ChainOptions(cb)}
                        </ul>
                    </div>
                </div>
            </div>
        </div>
    );
};