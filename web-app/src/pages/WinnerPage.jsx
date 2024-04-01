import { useParams } from "react-router"

const WinnerPage = () => {

    const { rName, winner } = useParams();

    return (
        <h1>{winner === "goodWins" ? "Bem Venceu!" : "Mal Venceu!"}</h1>
    )
}

export default WinnerPage;